const base = '/sap/zgantt';
const Link = ReactRouter.Link;

function handleError(evt, callback, json) {
  if (evt.target.status === 200) {
    if (json === true) {
      callback(JSON.parse(evt.target.responseText).DATA);
    } else {
      callback(evt.target.responseText);
    }
  } else {
    alert("REST call failed, status: " + evt.target.status);
  }
}

class REST {
  static root = base + "/rest/";

  static listProjects(callback) {
    this.get("projects", callback);
  }

  static listTasks(projectId, callback) {
    this.get("tasks/" + projectId, callback);
  }

  static get(folder, callback, json = true) {
    let oReq = new XMLHttpRequest();
    oReq.addEventListener("load", (evt) => { handleError(evt, callback, json); });
    oReq.open("GET", this.root + folder);
    oReq.send();
  }

  static post(folder, callback, data) {
    let oReq = new XMLHttpRequest();
    oReq.addEventListener("load", (evt) => { handleError(evt, callback, false); });
    oReq.open("POST", this.root + folder);
    oReq.send(JSON.stringify(data));
  }    
}

class NoMatch extends React.Component {
  render() {
    return (<h1>router, no match</h1>);
  }
}
            
class Spinner extends React.Component {
  render() {
    return (<div className="sk-rotating-plane"></div>);
  }
}  

class TaskFields extends React.Component {
  data;
            
  render() { 
    return (<div>
      DESCRIPTION<br />
      ESTIMATE<br />
      ACTUAL_START<br />
      ACTUAL_END<br />
      START_TIME<br />
      Dependencies<br />
      </div>); 
  }
}
            
class NewTask extends React.Component {     
  render() {
    return (<div>
            <h1>Project { this.props.params.project } - New Task</h1>
            todo
            <TaskFields />
            </div>);
  }
}             
            
class DisplayTask extends React.Component {     
  render() {
    return (<div>
            <h1>Project { this.props.params.project } - Task { this.props.params.task }</h1>
            display
            <TaskFields />
            </div>);
  }
}              

class EditTask extends React.Component {     
  render() {
    return (<div>
            <h1>Project { this.props.params.project } - Task { this.props.params.task }</h1>
            edit
            <TaskFields />
            </div>);
  }
}             
            
class Project extends React.Component {
  constructor(props) {
    super(props);
    this.state = {result: null};
    REST.listTasks(this.props.params.project, this.callback.bind(this));      
  }              

  callback(d) {
    this.setState({result: d});
  }        

  renderItem(i) {
    let link = this.props.params.project + "/" + i.HEADER.TASK_ID;
    return (<tr>
            <td><Link to={ link }>{ i.HEADER.TASK_ID } { i.HEADER.DESCRIPTION }</Link></td>
            <td><Link to={ link + "/edit" }>edit</Link></td>
            </tr>);
  }      
  
  renderItemList() {
    return (<table>{ this.state.result.map(this.renderItem.bind(this)) }</table>);
  }      
      
  render() {
    return (<div>
            <h1>Project { this.props.params.project }</h1>
            {this.state.result==null?<Spinner />:this.renderItemList()}
            <br />
            <Link to={ this.props.params.project + "/new" }>New task</Link>
            </div>);
  }
}            
            
class ProjectList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {result: null};
    REST.listProjects(this.callback.bind(this));      
  }            

  callback(d) {
    this.setState({result: d});
  }      

  renderItem(i) {
    return (<div><Link to={ i.PROJECT_ID }>{ i.PROJECT_ID } { i.DESCRIPTION }</Link></div>);
  }
      
  render() {    
    return (<div><h1>Project List</h1>
      {this.state.result==null?<Spinner />:this.state.result.map(this.renderItem.bind(this))}
      </div>);
  }
}                  
      
class Router extends React.Component {
        
  render() { 
    const history = ReactRouter.useRouterHistory(History.createHistory)({ basename: base });

    return (
      <ReactRouter.Router history={history} >
        <ReactRouter.Route path="/">
          <ReactRouter.IndexRoute component={ProjectList} />
          <ReactRouter.Route path=":project">
            <ReactRouter.IndexRoute component={Project} />
            <ReactRouter.Route path="new">
              <ReactRouter.IndexRoute component={NewTask} />
            </ReactRouter.Route>       
            <ReactRouter.Route path=":task">
              <ReactRouter.IndexRoute component={DisplayTask} />
              <ReactRouter.Route path="edit">
                <ReactRouter.IndexRoute component={EditTask} />
              </ReactRouter.Route>           
            </ReactRouter.Route>     
          </ReactRouter.Route>
        </ReactRouter.Route>
        <ReactRouter.Route path="*" component={NoMatch} />
      </ReactRouter.Router>);
  }
}
      
ReactDOM.render(<Router />, document.getElementById('app'));