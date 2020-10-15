var path = require("path");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const HardSourceWebpackPlugin = require('hard-source-webpack-plugin');

const outputPath = path.resolve(__dirname + '/dist');

const mainConfig = {
    target: 'electron-main',
    entry: './src/main.ts',
    output: {
        path: outputPath,
        filename: 'main.js'
    },
    module: {
        rules: [
            {
                test: /\.s[ac]ss$/i,
                use: [
                    'style-loader',
                    'css-loader',
                    // "postcss-loader",
                    'sass-loader',
                ],
            },
            {
                test:    /\.html$/,
                exclude: /node_modules/,
                loader:  'file-loader?name=[name].[ext]',
            },
            {
                test: /\.ts$/,
                use: 'ts-loader',
            },
            
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'url-loader?limit=10000&mimetype=application/font-woff',
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'file-loader',
            },
            {
                test: /\.html$/,
                loader: "html-loader"
            }
        ],
    },


    devtool: "source-map",
    externals: {
        fsevents: "require('fsevents')"
    },
    node: {
        __dirname: false
    },
    plugins: [
        new HardSourceWebpackPlugin()
    ]
};

const rendererConfig = {
    target: 'electron-renderer',
    entry: './src/renderer',
    output: {
        path: outputPath,
        filename: 'renderer.js',
    },

    module: {
        rules: [
            {
                test: /\.s[ac]ss$/i,
                use: [
                    'style-loader',
                    'css-loader',
                    // "postcss-loader",
                    'sass-loader',
                ],
            },
            {
                test:    /\.html$/,
                exclude: /node_modules/,
                loader:  'file-loader?name=[name].[ext]',
            },
            {
                test: /\.ts$/,
                use: 'ts-loader',
            },
            {
                test:    /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader:  'elm-webpack-loader?verbose=true',
                options: {
                    debug: true,
                }
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'url-loader?limit=10000&mimetype=application/font-woff',
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'file-loader',
            },
            {
                test: /\.html$/,
                loader: "html-loader"
            }

        ],
        noParse: /\.elm$/,
    },

    resolve: {
        extensions: [
            '.ts', '.js',
        ],
    },

    watch: true,
    watchOptions: {
        aggregateTimeout: 200,
        poll: 500,
        ignored: '**/.#*'
    },

    devServer: {
        inline: true,
        contentBase: './dist',
        stats: { colors: true },
    },
    devtool: "source-map",
    plugins: [
        new HtmlWebpackPlugin({
            title: "Cel Animate"
        }),
        new HardSourceWebpackPlugin()
    ],

};

module.exports = [mainConfig, rendererConfig];
