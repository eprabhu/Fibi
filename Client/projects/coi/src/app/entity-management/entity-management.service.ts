import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';
import { NameObject } from '../admin-dashboard/admin-dashboard.service';


@Injectable(
  {providedIn: 'root'}
  )

export class EntityManagementService {

  constructor(private _commonService: CommonService, private _http: HttpClient) { }

  sort: any;
  coiRequestObject = new EntityDashboardRequest();
  entityDashDefaultValues = new EntityDashDefaultValues();
  isShowEntityNavBar = false;
  searchDefaultValues: NameObject = new NameObject();
  sortCountObject: SortCountObj = new SortCountObj();
  relationshipDashboardRequest = new RelationshipDashboardRequest();
  isAdvanceSearch: any;
  statusCodes: any = [
    {code: 'Y', description: 'Active'},
    {code: 'N', description: 'Inactive'}
  ];
  concurrentUpdateAction = '';

  getAllSystemEntityList(params) {
    return this._http.post(this._commonService.baseUrl + '/getAllSystemEntityList', params);
  }

  getEntityDetails(entityId) {
    return this._http.get(`${this._commonService.baseUrl}/getEntityDetails/${entityId}`);

  }
  getEntityLookUp() {
    return this._http.get(this._commonService.baseUrl + '/loadSFILookups');
  }

  saveOrUpdateCOIEntity(params) {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiEntity', params);
  }

  getPersonEntityDashboard(params) {
    return this._http.post(this._commonService.baseUrl + '/getPersonEntityDashboard', params);
  }

  activateInactivate(params) {
    return this._http.put(this._commonService.baseUrl + '/entity/activateInactivate', params);
  }

  getApplicableQuestionnaire(requestObject: any) {
    return this._http.post(`${this._commonService.fibiUrl}/getApplicableQuestionnaire`, requestObject);
  }

  getRelationshipTypes() {
    return this._http.get(this._commonService.baseUrl + '/entity/relationshipTypes');
  }

  approveEntity(params) {
    return this._http.put(this._commonService.baseUrl + '/entity/approval', params);
  }

  entityHistory(params) {
    return this._http.post(this._commonService.baseUrl + '/entity/history', params);
  }

}
export class EntityDashboardRequest {
  property1 = '';
  isDownload = false;
  property2 = '';
  property20 = [];
  property21 = [];
  property22 = [];
  property24 = [];
  property18 = false;
  property19 = false;
  sort: any = {'updateTimeStamp': 'desc'};
  tabName = '';
  advancedSearch = 'L';
  currentPage = 1;
  pageNumber = 20;
}
export class RelationshipDashboardRequest {
  property1 = null;
  property2 = null;
  property3 = null;
  property4 = [];
  property5 = null;
  sort: any = {'updateTimeStamp': 'desc'};
  filterType = null;
  currentPage = 1;
  pageNumber = 20;
  id = null;
}

export class EntityDashDefaultValues {
  entitySearch = '';
  countrySearch = '';
}

export class SortCountObj {
  name = 0;
  entityType = 0;
  riskLevel = 0;
  country = 0;
  updateTimeStamp = 2;
}
