**NB**: This currently only works well on Clozure CL. Portability improvements coming soon.

# Opt-Check

This small library is intended to keep your optimizations in check.

There are two primary use cases.

## 1. Keeping Optimizations in Check

Often you start out with some very simple code, then making it efficient ends up complicating the hell out of it. When this happens, the original intent can get buried in the details. Opt-Check is meant to help prevent that from happening.

```common-lisp
(check-opt (simple-but-inefficient-thing)
           (complicated-but-efficient-equivalent-thing))
```

If this is in your code, the default behavior is to do the complicated-but-efficient thing. However, you can also tell the system to run the simple thing instead, so you can benchmark your system at a high level and see how much your optimizations are helping.

You can also have the system run _both_ versions and collect performance stats. This allows you to check the optimization benefits on a case-by-case basis, and make sure that your optimizations are still effective after other changes to the system (switching CL implementations, upgrading, etc.).

## 2. Checking Optimizations

This can be used as a benchmarking suite, much like a test suite. If you implement a new data structure, for example, you can use this to show how it compares to existing alternatives. Since the actual performance comparison will depend on the architecture, CL implementation, and versions of everything, something like this is necessary for potential users to see how it will behave in their specific case, rather than just the one place you tested it.

```common-lisp
(clear)
(check-opt (nth 5 list)
           (aref vector 5))
(report)
```
