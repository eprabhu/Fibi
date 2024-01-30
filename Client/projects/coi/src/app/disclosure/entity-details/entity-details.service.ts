import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../../app/common/services/common.service';
import { BehaviorSubject, Subject } from 'rxjs';

@Injectable()
export class EntityDetailsService {

  globalSave$: Subject<any> = new Subject<any>(); // to save questionnaire
  isRelationshipQuestionnaireChanged = false; //to track changes in questionnaire details.
  isAdditionalDetailsChanged = false; //to track changes in SFI details

  $openQuestionnaire = new Subject(); //to open questionnaire
  $saveQuestionnaireAction = new Subject(); //to trigger save action in questionnaire.
  $addOrDeleteRelation = new Subject(); //to trigger add or delete action in SFI details.
  canMangeSfi = false; //to check SFI edit permission.
  unSavedSections = []; //to store unsaved section details/
  relationshipCompletedObject: any = {}; // to store relationship code for complete and incomplete questionnaire.
  concurrentUpdateAction = ''; //store concurrent action
  activeRelationship : any; // active relationship type code.
  definedRelationships = []; //currently added relationships details.
  allAvailableRelationships = []; //relationship lookup
  remainingRelationships = []; //remaining relationship lookup
  $triggerAddRelationModal = new Subject(); //to open add relationship modal.
  $emitUnsavedChangesModal = new Subject() // to open unsaved changes modal
  activeTab: 'QUESTIONNAIRE' | 'RELATION_DETAILS' | 'HISTORY' = 'QUESTIONNAIRE'; //currently active Tab
  toBeActiveTab: 'QUESTIONNAIRE' | 'RELATION_DETAILS' | 'HISTORY'; //currently selected tab to set as active.
  currentVersionDetails: any = {}; //store current version details
  currentRelationshipQuestionnaire: any; // currently selected questionnaire
  groupedRelations = {}; //grouped available relations
  isVersionChange = false; //to track version change
  $triggerAddRelation = new Subject(); //to open add relation.
  
  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  getSFIDetails(coiFinancialEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getSFIDetails/${coiFinancialEntityId}`);
  }

  checkFormCompleted(personEntityId) {
    return this._http.patch(`${this._commonService.baseUrl}/personEntity/checkFormCompleted/${personEntityId}`, {});
  }

  saveOrUpdateCoiFinancialEntityDetails(params) {
    return this._http.post(this._commonService.baseUrl + '/personEntity/addRelationship', params);
  }

  async addSFILookUp(): Promise<any> {
    return this._http.get(`${this._commonService.baseUrl}/getRelationshipLookup`).toPromise();
  }

  getCoiEntityDetails(personEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getCoiEntityDetails/${personEntityId}`);
  }

  getRelationshipEntityDetails(personEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/personEntity/${personEntityId}`);
  }

  getPersonEntityRelationship(params) {
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

  sfiHistory(params) {
    return this._http.post(this._commonService.baseUrl + '/personEntity/history', params);
  }

  getSfiVersion(personEntityNumber) {
    return this._http.get(`${this._commonService.baseUrl}/personEntity/versions/${personEntityNumber}`);
  }

}

export function groupBy(jsonData, key, innerKey) {
    return jsonData.reduce((relationsTypeGroup, item) => {
        (relationsTypeGroup[item[key][innerKey]] = relationsTypeGroup[item[key][innerKey]] || []).push(item);
        return relationsTypeGroup;
    }, {});
}
