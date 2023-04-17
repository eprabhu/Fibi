import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';

@Injectable({
  providedIn: 'root'
})
export class EntityManagementService {

  constructor(private _commonService: CommonService, private _http: HttpClient) { }

  coiRequestObject = new CoiDashboardRequest();
  isShowEntityNavBar = false;
  relationshipDashboardRequest = new RelationshipDashboardRequest();


  getAllSystemEntityList() {
    return this._http.get(`${this._commonService.baseUrl}/getAllSystemEntityList`);
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

  getPersonEntityDetails(prams) {
    return this._http.post(this._commonService.baseUrl + '/getPersonEntityDetails', prams)
  }
}
export class CoiDashboardRequest {
  isDownload = false;
  property1 = '';
  property2 = [];
  property3 = [];
  property4 = [];
  property5 = null;
  property6 = null;
  property7 = null;
  sort: any = {};
  sortBy:'updateTimeStamp'
  tabName = '';
  advancedSearch = 'L';
  currentPage = 1;
}
export class RelationshipDashboardRequest{
  isDownload = false;
  property1 = '';
  property2 = null;
  property3 = null;
  property4 = null
  property5 = null;
  property6 = [];
  sort: any = {};
  sortBy:'updateTimeStamp'
  tabName = '';
  advancedSearch = 'L';
  currentPage = 1;

}
