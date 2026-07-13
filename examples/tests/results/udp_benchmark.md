
UDP communication benchmark (127.0.0.1, base port 19300)

| Scenario | Payload B | Messages | Wall ms | Sent | Recv | Loss % | msg/s | Mbit/s | p50 ms | p99 ms | Status |
|----------|-----------|----------|---------|------|------|--------|-------|--------|--------|--------|--------|
| oneway |  64 |  500 | 1.05 |  500 |  500 | 0.0 | 522466.04 | 368.19 | - | - | PASS |
| oneway |  256 |  500 | 1.02 |  500 |  500 | 0.0 | 492611.19 | 1103.45 | - | - | PASS |
| oneway |  1024 |  250 | 1.20 |  250 |  250 | 0.0 | 310174.32 | 2600.50 | - | - | PASS |
| oneway |  4096 |  125 | 1.46 |  125 |  125 | 0.0 | 229358.21 | 7560.37 | - | - | PASS |
| burst |  256 |  500 | 1.04 |  500 |  500 | 0.0 | 515996.13 | 1156.17 | - | - | PASS |
| roundtrip |  256 |  100 | 108.16 |  100 |  100 | 0.0 | 927.28 | 0 | 0 | 0 | PASS |
| stress-1t |  256 |  200 | 1.37 |  200 |  200 | 0.0 | 315457.41 | 707.38 | - | - | PASS |

Thresholds: oneway/burst loss <= 2%, stress loss <= 10% (full) / 15% (quick),
            roundtrip ping-pong loss <= 2% and p99 <= 5 ms (256 B, full mode).
