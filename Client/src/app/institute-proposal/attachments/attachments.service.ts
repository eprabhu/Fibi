import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class AttachmentsService {

  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  getProposalAttachments(proposalId) {
    return this._http.get(`${this._commonService.baseUrl}/getInstituteProposalAttachments/${proposalId}`);
  }

  deleteAttachment(proposalId, attachmentId) {
    return this._http.delete(`${this._commonService.baseUrl}/deleteIPAttachment/${proposalId}/${attachmentId}`);
  }

  downloadProposalAttachment(attachmentId) {
    return this._http.get(this._commonService.baseUrl + '/downloadInstituteProposalAttachment', {
      headers: new HttpHeaders().set('attachmentId', attachmentId.toString()),
      responseType: 'blob'
    });
  }

  downloadDevProposalAttachments(attachmentId) {
    return this._http.get(this._commonService.baseUrl + '/downloadProposalAttachment', {
      headers: new HttpHeaders().set('attachmentId', attachmentId.toString()),
      responseType: 'blob'
    });
  }

  addProposalAttachment(uploadedFile, proposalId, newAttachments) {
    const formData = new FormData();
    for (const file of uploadedFile) {
      formData.append('files', file, file.name);
    }
    formData.append('formDataJson', JSON.stringify({
      'proposalId': proposalId, 'instituteProposalAttachments': newAttachments
    }));
    return this._http.post(this._commonService.baseUrl + '/addIPAttachment', formData);
  }

  updateIPAttachmentDetails(data) {
    return this._http.post(this._commonService.baseUrl + '/updateIPAttachmentDetails', data);
  }

}
