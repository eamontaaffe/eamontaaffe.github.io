let component = ReasonReact.statelessComponent("Title");

module Query = [%graphql {|
  query IndexQuery {
    site {
      siteMetadata {
        title
      }
    }
  }
|}];

[@bs.deriving abstract]
type siteMetadata = {
  title: string,
};

[@bs.deriving abstract]
type site = {
  siteMetadata: siteMetadata,
};

[@bs.deriving abstract]
type data = {
  site: site,
};

let make = (_children) => {
  ...component,
  render: (_self) =>
    <Gatsby.StaticQuery
     query={Query.make()##query}
     render={(~data, _self) => {
       let title = data |. site |. siteMetadata |. title;
       <div>{ReasonReact.string(title)}</div>
     }}
     />,
};
