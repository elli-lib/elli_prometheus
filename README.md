# elli_prometheus

[![Hex.pm][hex badge]][hex package]
[![Documentation][docs badge]][docs]
[![Erlang][erlang badge]][erlang downloads]
[![Travis CI][build badge]][travis link]
[![Coveralls][coverage badge]][coveralls link]
[![BSD-3 License][license badge]](LICENSE)

*[Elli][] middleware for collecting stats via [Prometheus][].*

## Metrics

How Elli represents timings:

```
request_start
    headers_start
    ...headers receiving &  parsing...
    headers_end

    body_start
    ...body receiving & parsing...
    body_end

    user_start
    ...callback code...
    user_end

   send_start
   ...sending reply....
   send_end
request_end
```

How Elli represents sizes:

Each request has `resp_headers` key and, depending on
response type, `resp_body` or `file` or `chunks` key.

- `resp_header` is always present and denotes response headers wire size;
- `resp_body` set for regular responses;
- `file` set for file responses;
- `chunks` set for chunked responses, wire size too.

Elli_prometheus exports the following metrics:

- `http_requests_total`, counter. Total count of requests;
- `http_request_duration_microseconds`, histogram. The difference between
   `request_end` and `request_start`;
- `http_request_headers_microseconds`, histogram. The difference between
  `headers_end` and `headers_start`;
- `http_request_body_microseconds`, histogram. The difference between
  `body_end` and `body_start`;
- `http_request_user_microseconds`, histogram. The difference between
  `user_end` and `user_start`;
- `http_request_send_microseconds`, histogram. The difference between
  `send_end` and `send_start`;

- `http_response_size_bytes`, summary. Total size of the response, includes
  headers, body|file|chuncks;
- `http_response_headers_size_bytes`, summary. Size of the response headers;
- `http_response_body_size`, summary.
  Size of the response body;

For failed requests:
- `http_requests_failed_total{reason}`, Total count of failed requests. Reasons:
  - `request_closed` - the client closes the connection when Elli is waiting for
  the next request;
  - `request_timeout` - the client times out when Elli is waiting for the request;
  - `request_parse_error` - the request is invalid and cannot be parsed or it
  contains a path Elli cannot parse or doesn't support;
  - `client_closed` - the client closes the connection or socket closed
  unexpectedly;
  - `client_timeout` - data can't be received within a timeout;
  - `bad_request` - Elli detects a request isn't well formatted or doesn't conform
  to the configured limits.
- `http_bad_requests_total{reason}` - Total count of `bad_request` errors.
  Reasons:
  - `too_many_headers`;
  - `body_size`.
- `http_client_closed_total{request_part}` - Total count of `client_closed` errors.
   Parts:
   - `receiving_headers`;
   - `receiving_body`;
   - `before_response`.
- `http_client_timeout_total{request_part}` Total count of `client_timeout` errors.
  Parts:
   - `receiving_headers`;
   - `receiving_body`.

Exporter metrics:

* `telemetry_scrape_duration_seconds`<br />
Type: summary.<br />
Labels: `registry`, `content_type`.<br />
Scrape duration.

* `telemetry_scrape_size_bytes`<br />
Type: summary.<br />
Labels: `registry`, `content_type`.<br />
Scrape size, not encoded.

* `telemetry_scrape_encoded_size_bytes`<br />
Type: summary.<br />
Labels: `registry`, `content_type`, `encoding`.<br />
Scrape size, encoded.

## Dependencies

`elli_prometheus` requires [Elli][] and [Prometheus][], but neither are included
in this project. It has been tested and is known to work with `{elli, "2.0.1"}`
and `{prometheus, "3.1.1"}`.

[hex badge]: https://img.shields.io/hexpm/v/elli_prometheus.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/elli_prometheus
[erlang badge]: https://img.shields.io/badge/erlang-%E2%89%A518.0-red.svg
[erlang downloads]: http://www.erlang.org/downloads
[build badge]: https://travis-ci.org/elli-lib/elli_prometheus.svg?branch=develop
[travis link]: https://travis-ci.org/elli-lib/elli_prometheus
[docs badge]: https://img.shields.io/badge/docs-edown-green.svg
[docs]: doc/README.md
[coverage badge]: https://coveralls.io/repos/github/elli-lib/elli_prometheus/badge.svg?branch=develop
[coveralls link]: https://coveralls.io/github/elli-lib/elli_prometheus?branch=develop
[license badge]: https://img.shields.io/badge/license-BSD--3-blue.svg

[Elli]: https://github.com/elli-lib/elli
[Prometheus]: https://github.com/deadtrickster/prometheus.erl
