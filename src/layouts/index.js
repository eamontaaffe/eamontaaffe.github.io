import React from 'react';
import "./index.scss";

function Index({ children, data }) {
  return (
    <div className="layout">
      <div className="title">
        {data.site.siteMetadata.title}
      </div>
      <div className="content">
        {children()}
      </div>
    </div>
  );
}

export const pageQuery = graphql`
  query IndexLayoutQuery {
    site {
      siteMetadata {
        title
      }
    }
  }
`;

export default Index;
