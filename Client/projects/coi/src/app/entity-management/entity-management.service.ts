import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';

@Injectable({
  providedIn: 'root'
})
export class EntityManagementService {

  constructor(private _commonService: CommonService, private _http: HttpClient) { }

  coiRequestObject = new EntityDashboardRequest();
  entityDashDefaultValues = new EntityDashDefaultValues();
  isShowEntityNavBar = false;
  relationshipDashboardRequest = new RelationshipDashboardRequest();
  statusCodes: any = [
    {code: 'Y', description: 'Active'},
    {code: 'N', description: 'Inactive'}
  ];

  getAllSystemEntityList(prams) {
    return this._http.post(this._commonService.baseUrl+'/getAllSystemEntityList',prams);
  }

  getEntityDetails(entityId) {
    return this._http.get(`${this._commonService.baseUrl}/getEntityDetails/${entityId}`);

  }
  getEntityLookUp() {
    return this._http.get(this._commonService.baseUrl + '/loadSFILookups');
  }

  saveOrUpdateCOIEntity(prams) {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiEntity', prams)
  }

  getPersonEntityDashboard(prams) {
    return this._http.post(this._commonService.baseUrl + '/getPersonEntityDashboard', prams)
  }
}
export class EntityDashboardRequest {
  isDownload = false;
  property1 = '';
  property2 = '';
  property20 = [];
  property21 = [];
  property22 = [];
  property18 = false;
  property19 = false;
  sort: any = {
    sortBy : 'updateTimeStamp'
  };
  tabName = '';
  advancedSearch = 'L';
  currentPage = 1;
  pageNumber = 20;
}
export class RelationshipDashboardRequest{
  property1 = null;
  property2 = null;
  property3 = null;
  property4 = []
  // property5 = null;
  // sort: any = {};
  // sortBy:'updateTimeStamp'
  filterType = null;
  currentPage = 1;
  pageNumber = 20;
  id = null;
}

export class EntityDashDefaultValues {
  entitySearch = '';
  countrySearch = ''
}
