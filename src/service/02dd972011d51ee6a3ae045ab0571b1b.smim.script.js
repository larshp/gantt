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

class Time {
  static get() {
    let value = new Date().toJSON().slice(0,16);
    return value;
  }
  static hoursToMilliseconds(hours) {
    return hours * 60 * 60 * 1000;
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

  static readTask(projectId, taskId, callback) {
    this.get("tasks/" + projectId + "/" + taskId, callback);
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
  constructor(props) {
    super(props);    
    this.state = {data: this.props.data};
  }         

  updateDescription(e) {
    let data = this.state.data;
    data.HEADER.DESCRIPTION = e.target.value;
    this.setState(data);
  }   
      
  updateEstimate(e) {
    let data = this.state.data;
    data.HEADER.ESTIMATE = e.target.value;
    this.setState(data);
  }     
      
  updateActualStart(e) {
    setActualStart(e.target.value);
  }        

  setActualStart(value) {
    let data = this.state.data;
    data.HEADER.ACTUAL_START = value?value:Time.get();
    this.setState(data);
  }      
      
  updateActualEnd(e) {
    setActualEnd(e.target.value);
  }        
      
  setActualEnd(value) {
    let data = this.state.data;
    data.HEADER.ACTUAL_END = value?value:Time.get();
    this.setState(data);
  }        
      
  updatePlannedStart(e) {
    let data = this.state.data;
    data.HEADER.PLANNED_START = e.target.value;
    this.setState(data);
  }        
      
  render() {     
    let readOnly = this.props.readOnly?"readonly":"";
        
    return (
      <table>
      <tr>
      <td>Description</td>
      <td><input type="text" value={this.state.data.HEADER.DESCRIPTION} onChange={ this.updateDescription.bind(this) } readOnly={ this.props.readOnly } /></td>
      <td></td>
      </tr>
      <tr>
      <td>Estimate</td>
      <td><input type="number" value={this.state.data.HEADER.ESTIMATE} onChange={ this.updateEstimate.bind(this) } readOnly={ this.props.readOnly } /></td>
      <td></td>
      </tr>
      <tr>
      <td>Actual start</td>
      <td><input type="datetime-local" value={this.state.data.HEADER.ACTUAL_START} onChange={ this.updateActualStart.bind(this) } readOnly={ this.props.readOnly } /></td>
      <td><input type="button" value="Set" onClick={ () => { this.setActualStart(); } } disabled={ this.props.readOnly } /></td>
      </tr>
      <tr>
      <td>Actual end</td>
      <td><input type="datetime-local" value={this.state.data.HEADER.ACTUAL_END} onChange={ this.updateActualEnd.bind(this) } readOnly={ this.props.readOnly } /></td>
      <td><input type="button" value="Set" onClick={ () => { this.setActualEnd(); } } disabled={ this.props.readOnly } /></td>
      </tr>
      <tr>
      <td>Planned start</td>
      <td><input type="datetime-local" value={this.state.data.HEADER.PLANNED_START} onChange={ this.updatePlannedStart.bind(this) } readOnly={ this.props.readOnly } /></td>
      <td></td>
      </tr>
      <tr>
      <td align="top">Dependencies</td>
      <td>
      <select name="dependencies" multiple disabled={ this.props.readOnly }>
      <option value="volvo" selected>Volvo</option>
      <option value="saab">Saab</option>
      <option value="opel">Opel</option>
      <option value="audi">Audi</option>
      </select>
      </td>
      <td></td>
      </tr>
      </table>
      ); 
  }
}
            
class CreateTask extends React.Component {
  data;

  constructor(props) {
    super(props);
    this.data = { HEADER: {} };
  }
          
  render() {
    return (<div>
            <h1>Project { this.props.params.project } - Create Task</h1>
            <TaskFields data={ this.data } />
            <Link to={ "todo" }>create</Link> 
            </div>);
  }
}             
            
class DisplayTask extends React.Component {
  constructor(props) {
    super(props);
    this.state = {result: null};
    REST.readTask(this.props.params.project, this.props.params.task, this.callback.bind(this));      
  }              

  callback(d) {
    this.setState({result: d});
  }  
      
  render() {
    let link = this.props.params.project + "/" + this.props.params.task;
    
    return (<div>
      <h1>Project { this.props.params.project } - Task { this.props.params.task }</h1>
      {this.state.result==null?<Spinner />:<TaskFields data={ this.state.result } readOnly={ true } />}
      <Link to={ link + "/edit" }>edit</Link>            
      </div>);
  }
}              

class EditTask extends React.Component {     
  constructor(props) {
    super(props);
    this.state = {result: null};
    REST.readTask(this.props.params.project, this.props.params.task, this.callback.bind(this));      
  }              

  callback(d) {
    this.setState({result: d});
  }  
    
  render() {
    return (<div>
      <h1>Project { this.props.params.project } - Task { this.props.params.task }</h1>
      {this.state.result==null?<Spinner />:<TaskFields data={ this.state.result } />}
      <Link to="todo">save</Link>  
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
    google.charts.setOnLoadCallback(this.drawChart);      
    return (<table>{ this.state.result.map(this.renderItem.bind(this)) }</table>);
  }      
      
  drawChart() {
    var data = new google.visualization.DataTable();
      
    data.addColumn('string', 'Task ID');
    data.addColumn('string', 'Task Name');
    data.addColumn('string', 'Resource');
    data.addColumn('date', 'Start Date');
    data.addColumn('date', 'End Date');
    data.addColumn('number', 'Duration');
    data.addColumn('number', 'Percent Complete');
    data.addColumn('string', 'Dependencies');
      
    data.addRows([
        ['39-01', '39-01', null, new Date(2015, 0, 1), null, Time.hoursToMilliseconds(2), 0, null],
        ['03-01', '03-01', null, null, null, Time.hoursToMilliseconds(6),  0, '39-01'],
        ['03-17', '03-17', null, null, null, Time.hoursToMilliseconds(2), 0, '39-01'],
        ['02-01', '02-01', null, new Date(2015, 0, 1), null, Time.hoursToMilliseconds(2), 0, null],
        ['03-32', '03-32', null, null, null, Time.hoursToMilliseconds(2), 0, '02-01'],
        ['FICO', 'FI-CO', null, new Date(2015, 0, 1), null, Time.hoursToMilliseconds(2),  0, null],
        ['03-02', '03-02', null, null, null, Time.hoursToMilliseconds(2), 0, 'FICO'],
        ['03-04', '03-04', null, null, null, Time.hoursToMilliseconds(2), 0, '03-02'],
      ]);
      
    var options = { height: 2000, width: 3000 };
    var chart = new google.visualization.Gantt(document.getElementById('chart_div'));
    chart.draw(data, options);       
  }            
            
  render() {
    return (<div>
      <h1>Project { this.props.params.project }</h1>
      {this.state.result==null?<Spinner />:this.renderItemList()}
      <br />
      <Link to={ this.props.params.project + "/create" }>Create task</Link>
      <br /><br /><br />
      <div id="chart_div"></div>
      </div>);
  }
}            

class CreateProject extends React.Component {
  constructor(props) {
    super(props);
    this.state = { data: { DESCRIPTION: "" } };
  }
  
  updateDescription(e) {
    let data = this.state.data;
    data.DESCRIPTION = e.target.value;
    this.setState(data);    
  }
  
  render() {
    return (<div>
      <h1>Create Project</h1>
      Description:<br />
      <input type="text" value={this.state.data.DESCRIPTION} onChange={ this.updateDescription.bind(this) } /><br />
      create(todo)
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
      <br />
      <Link to="create">Create Project</Link>
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
            <ReactRouter.Route path="create">
              <ReactRouter.IndexRoute component={CreateProject} />
            </ReactRouter.Route>        
          <ReactRouter.Route path=":project">
            <ReactRouter.IndexRoute component={Project} />
            <ReactRouter.Route path="create">
              <ReactRouter.IndexRoute component={CreateTask} />
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