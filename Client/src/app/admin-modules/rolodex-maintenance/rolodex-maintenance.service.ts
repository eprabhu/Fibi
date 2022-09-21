
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class RolodexMaintenanceService {

  endpointSearchOptions: any = {
    contextField: '',
    formatString: '',
    path : ''
};

  constructor(private _http: HttpClient, private _commonService: CommonService) { }
  fetchRolodex( searchString) {
    return this._http.get( this._commonService.baseUrl + '/findRolodex' + '?searchString=' + searchString);
  }
  getRolodexData(rolodexId) {
    return this._http.post(this._commonService.baseUrl + '/getRolodexDetailById', {'rolodexId': rolodexId});
  }
  fetchOrganization( searchString) {
    return this._http.get( this._commonService.baseUrl + '/findOrganizations' + '?searchString=' + searchString);
    }
  saveRolodexData(sponsor) {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateRolodex', sponsor);
  }
  fetchRolodexData(params){
    return this._http.post(this._commonService.baseUrl + '/getAllRolodexes', params);
  }
  fetchSponsors( searchString) {
    return this._http.get( this._commonService.baseUrl + '/findSponsors' + '?searchString=' + searchString);
  }
    /**
   * @param  {} contextField
   * @param  {} formatString
   * @param  {} path
   *returns the endpoint search object with respect to the the inputs.
   */
  setSearchOptions(contextField, formatString, path) {
    this.endpointSearchOptions.contextField = contextField;
    this.endpointSearchOptions.formatString = formatString;
    this.endpointSearchOptions.path = path;
    return JSON.parse(JSON.stringify(this.endpointSearchOptions));
  }

}
