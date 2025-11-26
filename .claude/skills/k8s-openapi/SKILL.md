---
name: k8s-openapi
description: Reference the Kubernetes API. Useful for understanding what is possible via the Kubernetes REST API.
---

1. Start a test cluster with: `make test-cluster-create`. ONLY USE THIS SKILL ON THIS TEST CLUSTER.
2. Use the new context only through `--context` and `--kubeconfig` on each `kubectl` command. Do not
   alter the config via `kubectl config`.
3. Start a proxy server with `kubectl proxy`
4. Discover available endpoints by querying `/openapi/v3`
5. Use the discovered endpoints to find out what you need to
6. When you have found your answer:
    - Stop the proxy process; and
    - Shut down the test cluster with `make test-cluster-stop`
