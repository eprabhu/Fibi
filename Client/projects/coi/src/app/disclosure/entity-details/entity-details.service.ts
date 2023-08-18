import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../../app/common/services/common.service';
import { BehaviorSubject, Subject } from 'rxjs';

@Injectable()
export class EntityDetailsService {

  previousURL = '';
  lookups: any;
  $entityDetailsTest = new BehaviorSubject<object>({});
  globalSave$: Subject<any> = new Subject<any>();
  isShowRelationButton: any;
  isRelationshipQuestionnaireChanged = false;
  isAdditionalDetailsChanged = false;

  $saveQuestionnaireAction = new Subject();
  $relationshipsDetails = new BehaviorSubject<object>({});
  isExpanded = false;
  isHoverEntityCard = false;
  canMangeSfi = false;
  $relationshipTabSwitch = new BehaviorSubject<object>(null)
  isSwitchCurrentTab = false;
  isShowHistoryInfo = true;
  unSavedSections = [];

  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  getSFIDetails(coiFinancialEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getSFIDetails/${coiFinancialEntityId}`);
  }

  saveOrUpdateCoiFinancialEntityDetails(params) {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiFinancialEntityDetails', params);
  }

  async addSFILookUp(tabName): Promise<any> {
    return this._http.get(`${this._commonService.baseUrl}/getRelationshipLookup/${tabName}`).toPromise();
  }

  getCoiEntityDetails(personEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getCoiEntityDetails/${personEntityId}`);
  }

  getRelationshipEntityDetails(personEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getPersonEntityDetails/${personEntityId}`);
  }

  getPersonEntityRelationship(params) {
    return this._http.post(this._commonService.baseUrl + '/getPersonEntityRelationship', params);
  }

  loadSFILookups() {
    return this._http.get(this._commonService.baseUrl + '/loadSFILookups');
  }

  entityRisk(params) {
    return this._http.post(this._commonService.baseUrl + '/entity/modifyRisk', params);
  }

  riskHistory(entityId) {
    return this._http.get(`${this._commonService.baseUrl}/entity/riskHistory/${entityId}`);
  }

  deletePersonEntityRelationship(personEntityRelId, personEntityId) {
    return this._http.delete(`${this._commonService.baseUrl}/personEntity/relationship/${personEntityRelId}/${personEntityId}`);
  }

  getApplicableQuestionnaire(requestObject: any) {
    return this._http.post(this._commonService.fibiUrl + '/getApplicableQuestionnaire', requestObject);
  }

  updateAdditionalDetails(params) {
    return this._http.put(this._commonService.baseUrl + '/personEntity', params);
  }

  modifyPersonEntity(params) {
    return this._http.post(this._commonService.baseUrl + '/personEntity/modify', params);
  }

}
