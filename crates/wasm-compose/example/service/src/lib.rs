use service::{Error, Request, Response, Service};

struct Component;

impl Service for Component {
    fn execute(req: Request) -> Result<Response, Error> {
        // Right now, generated types aren't shared for bindings, so we
        // have to manually convert between the different types; eventually
        // this will not be necessary and we can call `backend::execute`
        // with the request passed to us.
        let headers: Vec<_> = req
            .headers
            .iter()
            .map(|(k, v)| (k.as_slice(), v.as_slice()))
            .collect();

        // Send the request to the backend and convert the response
        backend::execute(backend::Request {
            headers: &headers,
            body: req.body.as_slice(),
        })
        .map(|r| Response {
            headers: r.headers,
            body: r.body,
        })
        .map_err(|e| match e {
            backend::Error::BadRequest => Error::BadRequest,
        })
    }
}

service::export!(Component);
