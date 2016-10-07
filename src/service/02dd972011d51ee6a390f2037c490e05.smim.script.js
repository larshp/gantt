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

  static listRepositories(callback) {
    this.get("list", callback);
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
      
  static put(folder, callback, data) {
    let oReq = new XMLHttpRequest();
    oReq.addEventListener("load", (evt) => { handleError(evt, callback, false); });
    oReq.open("PUT", this.root + folder);
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
           
class Front extends React.Component {
  render() {    
    return (<div>hello world</div>);
  }
}                  
      
class Router extends React.Component {
        
  render() { 
    const history = ReactRouter.useRouterHistory(History.createHistory)({ basename: base });

    return (
      <ReactRouter.Router history={history} >
        <ReactRouter.Route path="/">
          <ReactRouter.IndexRoute component={Front} />
        </ReactRouter.Route>
        <ReactRouter.Route path="*" component={NoMatch} />
      </ReactRouter.Router>);
  }
}
      
ReactDOM.render(<Router />, document.getElementById('app'));