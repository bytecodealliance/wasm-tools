cargo_component_bindings::generate!();

use bindings::{
    example::service::handler as downstream,
    exports::example::service::handler::{Error, Handler, Request, Response},
};
use flate2::{write::GzEncoder, Compression};
use std::io::Write;

struct Component;

impl Handler for Component {
    fn execute(req: Request) -> Result<Response, Error> {
        // Send the request to the downstream service
        let mut response = downstream::execute(&downstream::Request {
            headers: req.headers,
            body: req.body,
        })
        .map(|r| Response {
            headers: r.headers,
            body: r.body,
        })
        .map_err(|e| match e {
            downstream::Error::BadRequest => Error::BadRequest,
        })?;

        // If the response is already encoded, leave it alone
        if response
            .headers
            .iter()
            .any(|(k, _)| k == b"content-encoding")
        {
            return Ok(response);
        }

        // Set the `content-encoding` header to `gzip`
        response
            .headers
            .push((b"content-encoding".to_vec(), b"gzip".to_vec()));

        // Compress the response body
        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder
            .write_all(&response.body)
            .map_err(|_| Error::BadRequest)?;
        response.body = encoder.finish().map_err(|_| Error::BadRequest)?;

        Ok(response)
    }
}
