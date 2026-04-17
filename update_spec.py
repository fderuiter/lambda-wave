with open("test/Spec.hs", "r") as f:
    content = f.read()
content = content.replace("import qualified Safety.WatchdogSpec", "import qualified Safety.WatchdogSpec\nimport qualified Control.UIRendererSpec")
content = content.replace("    describe \"Safety.Watchdog\" Safety.WatchdogSpec.spec", "    describe \"Safety.Watchdog\" Safety.WatchdogSpec.spec\n    describe \"Control.UIRenderer\" Control.UIRendererSpec.spec")
with open("test/Spec.hs", "w") as f:
    f.write(content)
