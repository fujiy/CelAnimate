var path = require("path");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const HardSourceWebpackPlugin = require('hard-source-webpack-plugin');
const TerserPlugin = require('terser-webpack-plugin');

const outputPath = path.resolve(__dirname + '/dist');

const config = {
    mode: 'development',
    entry: './src/renderer.ts',
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
                    {
                        loader: 'postcss-loader',
                        options: {
                            postcssOptions: {
                                ident: 'postcss',
                                plugins: [
                                    require('tailwindcss'),
                                    require('autoprefixer'),
                                ],
                            },
                        }
                    },
                    'sass-loader',
                ],
            },
            {
                test:    /\.html$/,
                exclude: /node_modules/,
                use:  'file-loader?name=[name].[ext]',
            },
            {
                test: /\.ts$/,
                use: 'ts-loader',
            },
            {
                test:    /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use:  'elm-webpack-loader?verbose=true',
                // options: {
                    // debug: true,
                // }
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                use: 'url-loader?limit=10000&mimetype=application/font-woff',
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                use: 'file-loader',
            },
            {
                test: /\.html$/,
                use: "html-loader"
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


}

const mainConfig = {
    mode: 'development',
    target: 'electron-main',
    entry: './src/main.ts',
    output: {
        path: outputPath,
        filename: 'main.js'
    },
    watch: true,
    module: {
        rules: [
            {
                test: /\.ts$/,
                use: 'ts-loader',
            },
            {
                test: /\.html$/,
                use: "html-loader"
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
    mode: 'development',
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
                    {
                        loader: 'postcss-loader',
                        options: {
                            postcssOptions: {
                                ident: 'postcss',
                                plugins: [
                                    require('tailwindcss'),
                                    require('autoprefixer'),
                                ],
                            },
                        }
                    },
                    'sass-loader',
                ],
            },
            {
                test:    /\.html$/,
                exclude: /node_modules/,
                use:  'file-loader?name=[name].[ext]',
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
                    debug: false,
                }
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                use: 'url-loader?limit=10000&mimetype=application/font-woff',
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                use: 'file-loader',
            },
            {
                test: /\.html$/,
                use: "html-loader"
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

    optimization: {
        // minimize: true,
        // minimizer: [new TerserPlugin({
        //     terserOptions: {
        //         ecma: 6,
        //         compress: true,
        //     }
        // })
        // ],
    },

    externals: [
        // 'electron',
    ]

};

module.exports = [mainConfig, rendererConfig];
// module.exports = config
