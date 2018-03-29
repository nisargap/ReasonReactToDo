/*************************************
** Nisarga Patel
** License: MIT
** Description: For learning purposes
**************************************/
type todo = {
  id: int,
  text: string,
  completed: bool,
};

type state = {todos: list(todo)};

/* Union types */
type action =
  | Add(string)
  | Check(int)
  | Delete(int);

/* Functional alias */
let toString = ReasonReact.stringToElement;

/* Since all variables are immutable by default
we have to create a reference */
let todoId = ref(0);

/* Incremenets the ref todoId by 1 the carrot signifies it's a ref */
let newTodo = text => {
  todoId := todoId^ + 1;
  {id: todoId^, completed: false, text};
};

/* List.map(mappingFunction, list) is essentially how mapping works,
  in this case we're each element if the ids match then invert the bool
  if they dont just return the original element */
let check = (id, todos) =>
  List.map(t => t.id === id ? {...t, completed: !t.completed} : t, todos);

/* List.filter filters out all elements that don't have that id */
let delete = (id, todos) =>
  List.filter(t => t.id !== id, todos);

let valueFromEvent = e : string => (
                                    e
                                    |> ReactEventRe.Form.target
                                    |> ReactDOMRe.domElementToObj
                                  )##value;

module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");

  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => "",
    reducer: (newTodo, _) => ReasonReact.Update(newTodo),
    render: ({state: todo, reduce}) =>
      <input
        className="input"
        value=todo
        _type="text"
        placeholder="What do you want todo?"
        onChange=(reduce(e => valueFromEvent(e)))
        onKeyDown=(
          (e) =>
            if (ReactEventRe.Keyboard.key(e) == "Enter") {
              onSubmit(todo);
              (reduce(() => ""))();
            }
        )
      />
  };
};

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~todo: todo, ~onToggle, ~clickDelete, _children) => {
    ...component,
    render: _self => 
      <div className="item" onClick=(_e => onToggle())>
        <input
          className="checkbox"
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean(todo.completed))
        />
        <label> (toString(todo.text)) </label>
        <input
          _type="button"
          className="btn-delete"
          value="x"
          onClick=(_e => clickDelete())
        />
      </div>
  };
};

/* We're using a reducer component */
let component = ReasonReact.reducerComponent("App");

/* the spread operator allows us to extend the component */
let make = _children => {
  ...component,
  initialState: () => {
    todos: []
  },
  reducer: (action, {todos}) =>
    switch action {
    | Add(text) => ReasonReact.Update({todos: [newTodo(text), ...todos]})
    | Check(id) => ReasonReact.Update({todos: check(id, todos)})
    | Delete(id) => ReasonReact.Update({todos: delete(id, todos)})
    },
  render: ({state: {todos}, reduce}) => {
    <div className="App">
      <h3>(toString("Todo App"))</h3>
      <Input onSubmit=(reduce(todo => Add(todo))) />
      <div className="todoList">
      (
        List.map(
          todo =>
            <TodoItem
              key=(string_of_int(todo.id))
              todo
              onToggle=(reduce(() => Check(todo.id)))
              clickDelete=(reduce(() => Delete(todo.id)))
            />,
          todos
        )
        |> Array.of_list
        |> ReasonReact.arrayToElement
      )
    </div>
    </div>
  }
}