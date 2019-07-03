Has anyone else realised that software versioning is really complicated? There are so many different approaches and implementations it can be hard to get your head around.

- [SemVer](https://semver.org/)
- [CalVer](https://calver.org/)
- [ZeroVer](https://0ver.org/)
- Snapshots
- Alpha/Beta/Release

There is a lot to take in. Then we have to deal with [*bad* implentations of versioning systems](https://gist.github.com/jashkenas/cbd2b088e20279ae2c8e).

Why don't we just use an incremental number? Anytime there is a minor version, patch, build, bugfix... anything, we can just increment the number.

The only thing that doesnt really work is major versions.

On that note, I think we should get rid of major versions. If you break the api of the project, give it a new name. There is nothing wrong with calling a project `awesome-project-2`. I think creating an entirely new project conveys the fact that any other project's is not supported.

So now what will this look like. Let's say I have a project `awesome-project` the sbt signature for this project will look like:

```scala
libraryDependencies += "eamontaaffe" %% "awesome-project" % "9"
```

The npm versioning will look like this.

```json
{
  "dependencies": {
    "awesome-project": "3 - 7"
  }
}
```

Let me know what you think, is this a terrible idea? ¯\_(ツ)_/¯
