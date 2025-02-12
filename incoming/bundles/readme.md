# Example Keter Applications

These subfolders contain (nearly) empty example keter applications to be made
into bundles for deployment. The goal of these different bundles is to ensure
that adding and removing bundles is reproducible.

To this end, a structure of hostnames are reserved by the child applications.
Each application also contains a script that will deploy it (on a system with
the recommended default keter location), test that particular bundle's config,
and un-deploy it.

Each bundle contains only a static "test" file that contains the name of the
bundle. That file is then exposed to a variety of hostnames:

```yaml
# foo.yaml
static-hosts:
    - host: foo.example.com
      root: ../static
    - host: foo.*.example.com
      root: ../static
    - host: foo.*.*.example.com
      root: ../static
    - host: foo.*.*.*.example.com
      root: ../static
```
