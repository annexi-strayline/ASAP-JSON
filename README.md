# ASAP-JSON
ANNEXI-STRAYLINE AURA Public (ASAP) Repository - High performance JSON parser/generator subsystem

This subsystem is a member of the larger [ASAP Repository](https://github.com/annexi-strayline/ASAP)

This subsystem provides a high-performance, high-security JSON parser/generator facility.

Design features:
* Finite state machine parser
* Unbounded codec uses a customized xxHash-based hash table strategy for fast key-based indexing of JSON objects
* Unbounded codec uses an advanced allocation strategy that ensures all memory allocations are both constant, and page sized, reducing memory fragmentation in long-running, high-throughput applications (such as in server environments).