let component = ReasonReact.statelessComponent("Index");

let make = (_children) => {
  ...component,
  render: (_self) =>
    <div>
    <Title />
    {ReasonReact.string("Yolo world!")}
    </div>,
};


let default =
  ReasonReact.wrapReasonForJs(
    ~component,
    _jsProps => make([||]),
  );
