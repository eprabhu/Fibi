import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../../../common/services/common.service';

@Injectable()
export class AttachmentService {

constructor(private _http: HttpClient, public _commonService: CommonService) { }

  loadIPAttachments(proposalId) {
    return this._http.get(`${this._commonService.baseUrl}/getInstituteProposalAttachments/${proposalId}`);
  }

  downloadProposalAttachment( attachmentId ) {
    return this._http.get( this._commonService.baseUrl + '/downloadProposalAttachment', {
        headers: new HttpHeaders().set( 'attachmentId', attachmentId.toString() ),
        responseType: 'blob'
    } );
  }
}
