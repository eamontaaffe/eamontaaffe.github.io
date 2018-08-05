module.exports = {
  siteMetadata: {
    title: 'Eamon',
  },
  plugins: [
    'gatsby-transformer-yaml',
    {
      resolve: 'gatsby-source-filesystem',
      name: 'events',
      options: {
        path: './src/events/',
      },
    },
  ],
};
