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
  isRelationshipQuestionnaireChanged = false;
  isAdditionalDetailsChanged = false;

  $openQuestionnaire = new Subject();
  $updateFormCompleted = new Subject()
  $saveQuestionnaireAction = new Subject();
  $relationshipsDetails = new BehaviorSubject<object>({});
  isExpanded = false;
  isHoverEntityCard = false;
  canMangeSfi = false;
  isSwitchCurrentTab = false;
  isShowHistoryInfo = true;
  unSavedSections = [];
  relationshipCompletedObject: any = {};
  concurrentUpdateAction = '';
  activeRelationship : any;
  definedRelationships = [];
  availableRelationships = [];
  $triggerAddRelationModal = new Subject();
  $emitUnsavedChangesModal = new Subject()
  selectedTab = 'QUESTIONNAIRE';
  clickedTab: 'QUESTIONNAIRE' | 'RELATION_DETAILS' | 'HISTORY';
  currentVersionDetails: any = {};
  currentRelationshipQuestionnaire: any;
  isChecked = {};
  groupedRelations = {};
  $triggerAddRelation = new Subject();

  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  getSFIDetails(coiFinancialEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getSFIDetails/${coiFinancialEntityId}`);
  }

  checkFormCompleted(personEntityId) {
    return this._http.patch(`${this._commonService.baseUrl}/personEntity/checkFormCompleted/${personEntityId}`, {});
  }

  saveOrUpdateCoiFinancialEntityDetails(params) {
    // return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiFinancialEntityDetails', params);
    return this._http.post(this._commonService.baseUrl + '/personEntity/addRelationship', params);
  }

  async addSFILookUp(): Promise<any> {
    return this._http.get(`${this._commonService.baseUrl}/getRelationshipLookup`).toPromise();
  }

  getCoiEntityDetails(personEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getCoiEntityDetails/${personEntityId}`);
  }

  getRelationshipEntityDetails(personEntityId) {
    // return this._http.get(`${this._commonService.baseUrl}/getPersonEntityDetails/${personEntityId}`);
    return this._http.get(`${this._commonService.baseUrl}/personEntity/${personEntityId}`);
  }

  getPersonEntityRelationship(params) {
    // return this._http.post(this._commonService.baseUrl + '/getPersonEntityRelationship', params);
    return this._http.post(this._commonService.baseUrl + '/personEntity/getRelationship', params);
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

  getCurrentId(personEntityNumber) {
    return this._http.get(`${this._commonService.baseUrl}/personEntity/${personEntityNumber}/latestVersion`);
  }

  sfiHistory(params) {
    return this._http.post(this._commonService.baseUrl + '/personEntity/history', params);
  }

  getSfiVersion(personEntityNumber) {
    return this._http.get(`${this._commonService.baseUrl}/personEntity/versions/${personEntityNumber}`);
  }

}

