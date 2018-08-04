module Graphql = {
  [@bs.deriving abstract]
  type eventsYaml = {
    [@bs.as "type"] _type: string,
  };

  [@bs.deriving abstract]
  type file = {
    absolutePath: string,
    childEventsYaml: eventsYaml,
  };

  [@bs.deriving abstract]
  type fileEdge = {
    node: file,
  };

  [@bs.deriving abstract]
  type fileConnection = {
    edges: array(fileEdge),
  };

  type error;
  
  [@bs.deriving abstract]
  type result('data) = {
    error: error,
    data: 'data,
  };

  type t('data) = [@bs] (string) => Js.Promise.t(result('data));
};

module BoundActionCreators = {
  type createPage;
  
  [@bs.deriving abstract]
  type t = {
    createPage: createPage,
  };
};

[@bs.deriving abstract]
type createPagesArgs('data) = {
  graphql: Graphql.t('data),
  boundActionCreators: BoundActionCreators.t,
};

let query = {|
   {
     events: allFile {
       edges {
         node {
           absolutePath
             childEventsYaml {
               type
             }
         }
       }
     }
   } |};

[@bs.deriving abstract]
type eventsData = {
  events: Graphql.fileConnection,
};

let aboutAggregate = (_a: boundActionCreators, ex: array(Graphql.fileEdge)) =>

let createPages = (
  args: createPagesArgs('data),
) => {
  open Js.Promise;
  
  let gql = args |> graphql;
  let actions = args |> boundActionCreators;
 
  gql(. query)
  |> then_((r: Graphql.result(eventsData)) => {
      let eventFiles = r |> Graphql.data |> events |> Graphql.edges;

      all([ aboutAggregate(actions, eventFiles) ]);
    })
};
