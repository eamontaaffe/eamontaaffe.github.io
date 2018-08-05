module StaticQuery = {
  [@bs.module "gatsby"]
    external reactClass: ReasonReact.reactClass = "StaticQuery";

  [@bs.deriving abstract]
  type jsProps('render) = {
    query: string,
    render: 'render,
  };

  let make = (~query, ~render, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass,
      ~props=jsProps(
        ~query=query,
        ~render=render
      ),
      children,
    );
};

module Link = {
  [@bs.module "gatsby-link"]
    external reactClass: ReasonReact.reactClass = "default";

  [@bs.deriving abstract]
  type props = {
    [@bs.as "to"] _to: string,
  };

  let make = (~_to, children) =>
    ReasonReact.wrapJsForReason(
      ~reactClass,
      ~props=props(~_to),
      children,
    );
};
