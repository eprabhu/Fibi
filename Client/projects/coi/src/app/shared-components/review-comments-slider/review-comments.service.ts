import {Injectable} from '@angular/core';
import {CommonService} from '../../common/services/common.service';
import {HttpClient, HttpHeaders} from '@angular/common/http';

@Injectable()
export class ReviewCommentsService {


    isEditParentComment = false;
    editParentCommentId = null;

    constructor(private _commonService: CommonService, private _http: HttpClient) {
    }

    getSectionsTypeCode(params) {
        return this._http.post(this._commonService.baseUrl + '/getCoiSectionsTypeCode', params);
    }

    getAdminDetails() {
        return this._http.get(this._commonService.baseUrl + '/adminGroup/adminPersons');
    }

    getCoiReviewComments(params) {
        return this._http.post(this._commonService.baseUrl + '/loadCoiReviewComments', params);

    }

    downloadAttachment(params) {
        return this._http.get(this._commonService.baseUrl + '/attachment/downloadDisclAttachment', {
            headers: new HttpHeaders().set('attachmentId', params.toString()),
            responseType: 'blob'
        });
    }

    deleteCOIAssignee(coiReviewCommentTagId) {
        return this._http.delete(`${this._commonService.baseUrl}/deleteCOIReviewCommentTags/${coiReviewCommentTagId}`);
    }

    deleteCOICommentAttachment(params) {
        return this._http.post(this._commonService.baseUrl + ' /attachment/deleteDisclAttachment', params);
    }

    deleteReviewComments(coiReviewCommentId) {
        return this._http.delete(`${this._commonService.baseUrl}/deleteCOIReviewComment/${coiReviewCommentId}`);
    }

    addCOIReviewComment(params: any, uploadedFiles) {
        const formData = new FormData();
        if (uploadedFiles) {
            uploadedFiles.forEach(file => {
                formData.append('files', file, file.name);
            });
        }
        formData.append('formDataJson', JSON.stringify(params));
        return this._http.post(this._commonService.baseUrl + '/addCOIReviewComment', formData);
    }

    getSfiDetails(params: any) {
        return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', params);
    }

    getProjectRelationships(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/getDisclosureRelations`, params);
    }

    getEntityProjectRelations(moduleCode, moduleId, id, status, personId) {
        if (moduleCode == 3) {
          return this._http.post(this._commonService.baseUrl + '/disclosure/project/relations', {
            'disclosureId': id,
            'proposalIdlinkedInDisclosure': moduleId,
            'disclosureStatusCode': status,
            'moduleCode': moduleCode,
            'moduleItemId': moduleId,
            'personId': personId,
          });
        } else {
          return this._http.post(this._commonService.baseUrl + '/disclosure/project/relations', {
            'disclosureId': id,
            'disclosureStatusCode': status,
            'moduleCode': moduleCode,
            'moduleItemId': moduleId,
            'personId': personId
          });
        }
      }

      loadDisclAttachTypes() {
        return this._http.get(this._commonService.baseUrl + '/loadDisclAttachTypes');
    }

}
