import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';
import { HttpClient } from '@angular/common/http';
import { HTTP_ERROR_STATUS } from '../app-constants';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class EntityDashboardService {

  constructor(private _commonService: CommonService, private _http: HttpClient) { }

  sort: any;
  entityDashDefaultValues = new EntityDashDefaultValues();
  isShowEntityNavBar = false;
  searchDefaultValues: NameObject = new NameObject();
  sortCountObject: SortCountObj = new SortCountObj();
  relationshipDashboardRequest = new RelationshipDashboardRequest();
  isAdvanceSearch: any;
  statusCodes: any = [
    {code: 'Y', description: 'Active'},
    {code: 'N', description: 'Inactive'},
    {code: 'D', description: 'Duplicate'},
  ];
  concurrentUpdateAction = '';
  entitySearchRequestObject = new EntityDashboardSearchRequest();
  sortCountObj: any = {};

  getAllSystemEntityList(params) {
    return this._http.post(this._commonService.baseUrl + '/entity/getEntityDashboardData', params).pipe(catchError((err) => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching Entity List failed. Please try again.');
        return of();
      }));
  }
}

export class EntityDashboardSearchRequest{
  [key: string]: any;
  entityDashboardData: any = {
      SORT_TYPE : {UPDATE_TIMESTAMP: 'DESC'},
      LIMIT: 20,
      PAGED: 0,
      TYPE: 'A',
      TAB_TYPE: 'ALL_ENTITY',
      UNLIMITED: false
  };
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

  export class NameObject {
    entityName = '';
    personName = '';
    departmentName = '';
    travelCountryName = '';
  }
