use clap::Parser;
use std::path::{Path, PathBuf};
use tide::{
    http::{
        headers::{HeaderName, HeaderValue},
        mime,
    },
    utils::After,
    Request, Response, StatusCode,
};
use wasmtime::{component::*, Config, Engine, Store};

wasmtime_component_macro::bindgen!({
    path: "../service.wit",
    world: "service"
});

/// Represents state stored in the tide application context.
///
/// This is so that a component is only parsed and compiled once.
/// Each request will create a new instance of the component.
#[derive(Clone)]
struct State {
    engine: Engine,
    component: Component,
}

impl State {
    fn new(path: impl AsRef<Path>) -> tide::Result<Self> {
        // Enable component model support in Wasmtime
        let mut config = Config::default();
        config.wasm_component_model(true);

        // Load the component from the given path
        let engine = Engine::new(&config)?;
        let component = Component::from_file(&engine, path)?;
        Ok(Self { engine, component })
    }
}

impl TryFrom<handler::Response> for tide::Response {
    type Error = tide::Error;

    fn try_from(r: handler::Response) -> Result<Self, Self::Error> {
        // Convert the service response to a tide response
        let mut builder = tide::Response::builder(StatusCode::Ok);
        for (name, value) in r.headers {
            builder = builder.header(
                HeaderName::from_bytes(name)?,
                HeaderValue::from_bytes(value)?,
            );
        }
        builder = builder.body(r.body);
        Ok(builder.build())
    }
}

impl handler::Error {
    fn into_tide(self) -> tide::Error {
        match self {
            Self::BadRequest => {
                tide::Error::from_str(StatusCode::BadRequest, "bad service request")
            }
        }
    }
}

/// WebAssembly component server.
///
/// A demonstration server that executes WebAssembly components.
#[derive(Parser)]
#[clap(name = "server", version = env!("CARGO_PKG_VERSION"))]
struct ServerApp {
    /// The path to the service component.
    #[clap(value_name = "SERVICE_PATH")]
    service: PathBuf,

    /// The port to listen on.
    #[clap(long, short = 'p', value_name = "PORT", default_value = "8080")]
    port: u16,
}

impl ServerApp {
    async fn run(self) -> tide::Result<()> {
        // Create a new server with state holding the service component.
        let mut app = tide::with_state(State::new(&self.service)?);
        app.with(driftwood::DevLogger);

        // Add a middleware that responds with error text for 500s
        app.with(After(|response: Response| async move {
            Ok(match response.status() {
                // For demonstration purposes, respond with the error text for debugging
                // Please, don't do this in production
                StatusCode::InternalServerError => Response::builder(500)
                    .content_type(mime::PLAIN)
                    .body(response.error().unwrap().to_string())
                    .build(),
                _ => response,
            })
        }));

        // App handles only POSTs to `/`
        app.at("/").post(Self::process_request);

        // Finally listen at the desired port
        let address = format!("localhost:{port}", port = self.port);
        println!("listening at http://{address}/");
        app.listen(address).await.map_err(Into::into)
    }

    async fn process_request(mut req: Request<State>) -> tide::Result {
        let body = req.body_bytes().await?;
        let headers = req
            .iter()
            .map(|(n, v)| (n.as_str().as_bytes(), v.as_str().as_bytes()))
            .collect::<Vec<_>>();

        // Create a new store for the request
        let state = req.state();
        let linker: Linker<()> = Linker::new(&state.engine);
        let mut store = Store::new(&state.engine, ());
        let (service, _) = Service::instantiate(&mut store, &state.component, &linker)?;
        service
            .handler
            .call_execute(
                &mut store,
                handler::Request {
                    headers: &headers,
                    body: &body,
                },
            )?
            .map(TryInto::try_into)
            .map_err(handler::Error::into_tide)?
    }
}

#[async_std::main]
async fn main() -> tide::Result<()> {
    // Parse the command line arguments and run the application
    ServerApp::parse().run().await
}
