import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';

import { CommonService } from '../../common/services/common.service';
import { GetSFIRequestObject } from '../coi-interface';

@Injectable()
export class CoiSummaryService {

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }

    getProjectRelationships(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/getDisclosureRelations`, params);
    }

    getEntityProjectRelations(moduleCode, moduleId, id, status, personId) {
      if (moduleCode == 3) {
        return this._http.post(this._commonService.baseUrl + '/getEntityProjectRelations', {
          'disclosureId': id,
          'proposalIdlinkedInDisclosure': moduleId,
          'disclosureStatusCode': status,
          'moduleCode': moduleCode,
          'moduleItemId': moduleId,
          'personId': personId,
        });
      } else {
        return this._http.post(this._commonService.baseUrl + '/getEntityProjectRelations', {
          'disclosureId': id,
          'disclosureStatusCode': status,
          'moduleCode': moduleCode,
          'moduleItemId': moduleId,
          'personId': personId
        });
      }
    }

    getSfiDetails(params: GetSFIRequestObject) {
        return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', params);
    }

    updateProjectConflictStatus(params: any) {
        return this._http.post(this._commonService.baseUrl + '/updateProjectConflictStatus', params);
    }

    loadProjectConflictHistory(disclosureDetailsId: any) {
        return this._http.get(`${this._commonService.baseUrl}/loadProjectConflictHistory/${disclosureDetailsId}`);
    }

}
