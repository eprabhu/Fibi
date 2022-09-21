import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { BehaviorSubject, Subject } from 'rxjs';
import { InstituteProposal } from '../institute-proposal-interfaces';

@Injectable()
export class InstituteProposalService {

  ipSectionConfig: any = {};

  isTriggerStatus = new Subject();
  isInstituteProposalDataChange = false;
  ipTitle: any;

  public instituteProposalData = new BehaviorSubject<InstituteProposal>(null);

  constructor(private _http: HttpClient, public _commonService: CommonService) { }

  loadProposalById(params) {
    return this._http.post(this._commonService.baseUrl + '/loadInstProposalById', params);
  }

  downloadProposalAttachment(attachmentId) {
    return this._http.get(this._commonService.baseUrl + '/downloadInstituteProposalAttachment', {
      headers: new HttpHeaders().set('attachmentId', attachmentId.toString()),
      responseType: 'blob'
    });
  }
  setInstituteProposalData(ipData) {
    this.instituteProposalData.next(ipData);
  }
  downloadProposalPersonAttachment(attachmentId) {
    return this._http.get(this._commonService.baseUrl + '/downloadProposalPersonAttachment', {
      headers: new HttpHeaders().set('attachmentId', attachmentId.toString()),
      responseType: 'blob'
    });
  }
  saveOrUpdateIpDetails(params) {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateInstituteProposal', params);
  }
  getPersonData(personId) {
    return this._http.post(this._commonService.baseUrl + '/getPersonDetailById', { 'personId': personId });
  }
  getRolodexData(rolodexId) {
    return this._http.post(this._commonService.baseUrl + '/getRolodexDetailById', { 'rolodexId': rolodexId });
  }

  createNewIPVersion(params) {
    return this._http.post(this._commonService.baseUrl + '/createNewIPVersion', params);
  }

  changeIpStatus(params) {
    return this._http.post(this._commonService.baseUrl + '/changeIPStatus', params);
  }

  submitIPVersion(params) {
    return this._http.post(`${this._commonService.baseUrl}/submitInstituteProposal`, params);
  }
  evaluateValidation(params) {
    return this._http.post(this._commonService.baseUrl + '/evaluateValidationRule', params).toPromise();
  }

  getLetterTemplates() {
    return this._http.get(this._commonService.baseUrl + '/letterTemplate/2');
}

  printInstituteProposal(params) {
  return this._http.post(this._commonService.baseUrl + '/generateIpReport', params, {responseType: 'blob'});
}
  deleteIPKeyword(proposalId, keywordId) {
    return this._http.delete( this._commonService.baseUrl + `/deleteIPKeyword/${proposalId}/${keywordId}`);
  }

  addScienceKeyword(params) {
    return this._http.post( this._commonService.baseUrl + '/addScienceKeyword', params );
  }

}
