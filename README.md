# Password generator

## Initial setup and first build

Compiling the [`purescript`](http://www.purescript.org) code, supposing [`nvm`](http://nvm.sh) is already installed, is as simple as typing:

    > nvm install --lts (14.16.0)
    > npm install -g npm
    > npm install -g purescript spago    parcel-bundler sass     yarn
    --               @0.14.0    @0.19.1  @1.12.4        @1.32.8  @1.22.10
    > yarn clean; yarn build
    > yarn docs
    > yarn develop-app


## VSCode integration

To support editing Purescript files, there are two useful VSCode plugins:
- PureScript IDE: https://github.com/nwolverson/vscode-ide-purescript
- PureScript Language Support: https://github.com/nwolverson/vscode-language-purescript

In order to have errors highlighted directly into VSCode, you need to set one option into the "PureScript IDE" module:
- "purescript.editorMode": true (Editor Mode: Whether to set the editor-mode flag on the IDE server)

To run the application, just type the following two commands in two different terminal windows:
- `yarn develop-purs`
- `yarn develop-app`

The first command will invoke `spago` to continuosly compile the PureScript files, while the second will start a web server to serve the application on a local port, reported by [`ParcelJS`]() logs:

    >> starting...
    Server running at http://localhost:1234 
    ✨  Built in 2.36s.


## Local packaged used

Being a work in progress (WIP), this project uses a few local dependencies, both to be able to access still unreleased versions of public libraries (eg Concur) and still completely unpublished (and far from finished) libraries (eg Forturna).

These are the commands to retrieve these libraries:

    > git clone https://github.com/clipperz/purescript-fortuna.git ../purescript-fortuna
    > git clone https://github.com/purescript-concur/purescript-concur-core.git ../purescript-concur-core
    > git clone	https://github.com/purescript-concur/purescript-concur-react.git ../purescript-concur-react
    > git clone https://github.com/ajnsit/purescript-formless-independent.git ../purescript-formless-independent



# Interesting links

- https://qiita.com/kimagure/items/08c59fa21adcd6968ae1
- https://stackoverflow.com/questions/37231474/typeclass-instance-with-row-type-in-instance-head
- https://github.com/paf31/24-days-of-purescript-2016/blob/master/10.markdown