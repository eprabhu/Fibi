import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';

@Injectable()

export class ScheduleListService {

constructor(private _http: HttpClient, private _commonService: CommonService) {}

dashboardRequestObject = {
  property1: '',
  property2: '',
  property3: '',
  property4: '',
  property5: '',
  property6: '',
  property7: '',
  property8: '',
  property9: '',
  property10: '',
  property11: '',
  property13: '',
  property14: '',
  pageNumber: 20,
  sortBy: 'updateTimeStamp',
  sort: {},
  reverse: 'DESC',
  tabIndex: null,
  userName: localStorage.getItem(''),
  personId: this._commonService.getCurrentUserDetail('personID'),
  currentPage: 1,
  isUnitAdmin: (this._commonService.getCurrentUserDetail('unitAdmin') === 'true'),
  unitNumber: this._commonService.getCurrentUserDetail('unitNumber'),
  provost: (this._commonService.getCurrentUserDetail('provost') === 'true'),
  reviewer: (this._commonService.getCurrentUserDetail('reviewer') === 'true'),
  isGrantAdmin: (this._commonService.getCurrentUserDetail('grantAdmin') === 'true'),
  isDEC: (this._commonService.getCurrentUserDetail('dec') === 'true'),
  isURC: (this._commonService.getCurrentUserDetail('urc') === 'true'),
  isResearcher: (this._commonService.getCurrentUserDetail('researcher') === 'true'),
  isIRBSecretariat: (localStorage.getItem('irbsecretariat') === 'true'),
  isHOD: (localStorage.getItem('hod') === 'true'),
  isOrttDirector: (localStorage.getItem('orttDirector') === 'true'),
  tabName: null,
  grantCallId: null,
  canCreateGrantCall: false,
  advancedSearch: 'L'
};
getDashboardObject() {
  this.dashboardRequestObject.isUnitAdmin = (this._commonService.getCurrentUserDetail('unitAdmin') === 'true');
  this.dashboardRequestObject.personId = this._commonService.getCurrentUserDetail('personID');
  this.dashboardRequestObject.unitNumber = this._commonService.getCurrentUserDetail('unitNumber'),
    this.dashboardRequestObject.userName = this._commonService.getCurrentUserDetail('userName');
  this.dashboardRequestObject.provost = (this._commonService.getCurrentUserDetail('provost') === 'true');
  this.dashboardRequestObject.reviewer = (this._commonService.getCurrentUserDetail('reviewer') === 'true');
  this.dashboardRequestObject.isGrantAdmin = (this._commonService.getCurrentUserDetail('grantAdmin') === 'true');
  this.dashboardRequestObject.isDEC = (this._commonService.getCurrentUserDetail('dec') === 'true');
  this.dashboardRequestObject.isURC = (this._commonService.getCurrentUserDetail('urc') === 'true');
  this.dashboardRequestObject.isResearcher = (this._commonService.getCurrentUserDetail('researcher') === 'true');
  this.dashboardRequestObject.isIRBSecretariat = (localStorage.getItem('irbsecretariat') === 'true');
  this.dashboardRequestObject.isHOD = (localStorage.getItem('hod') === 'true');
  this.dashboardRequestObject.isOrttDirector = (localStorage.getItem('orttDirector') === 'true');
  return this.dashboardRequestObject;
}

getDashboardList(params) {
  return this._http.post(this._commonService.baseUrl + '/fibiDashBoard', params);
}

}
