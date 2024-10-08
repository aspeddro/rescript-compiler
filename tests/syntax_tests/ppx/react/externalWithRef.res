@@jsxConfig({version: 4, mode: "classic"})

module V4C = {
  @module("componentForwardRef") @react.component
    external make: (
      ~x: string,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element = "component"
}

module type V4CType = {
  @module("someModule") @react.component
    external make: (
      ~x: string,
      ~y: string,
    ) => React.element = "component"
}

@@jsxConfig({version: 4, mode: "automatic"})

module V4C = {
  @module("componentForwardRef") @react.component
    external make: (
      ~x: string,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element = "component"
}
