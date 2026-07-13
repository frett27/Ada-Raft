
UDP communication benchmark (127.0.0.1, base port 19300)

| Scenario | Payload B | Messages | Wall ms | Sent | Recv | Loss % | msg/s | Mbit/s | p50 ms | p99 ms | Status |
|----------|-----------|----------|---------|------|------|--------|-------|--------|--------|--------|--------|
| oneway |  64 |  500 | 1.10 |  500 |  500 | 0.0 | 458716.41 | 323.07 | - | - | PASS |
| oneway |  256 |  500 | 1.10 |  500 |  500 | 0.0 | 455789.47 | 1021.04 | - | - | PASS |
| oneway |  1024 |  250 | 1.27 |  250 |  250 | 0.0 | 342466.26 | 2871.24 | - | - | PASS |
| oneway |  4096 |  125 | 1.39 |  125 |  125 | 0.0 | 204583.35 | 6743.05 | - | - | PASS |
| burst |  256 |  500 | 1.16 |  500 |  500 | 0.0 | 431035.51 | 966.49 | - | - | PASS |
| roundtrip |  256 |  100 | 109.22 |  100 |  100 | 0.0 | 919.26 | 0 | 0 | 0 | PASS |
| stress-1t |  256 |  200 | 1.42 |  200 |  200 | 0.0 | 344234.10 | 771.09 | - | - | PASS |

Thresholds: oneway/burst loss <= 2%, stress loss <= 10% (full) / 15% (quick),
            roundtrip ping-pong loss <= 2% and p99 <= 5 ms (256 B, full mode).
