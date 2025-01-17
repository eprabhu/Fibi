import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { RO } from '../../coi-interface';

@Injectable()
export class CoiSummaryService {

  activeSubNavItemId = '';

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }

    getEntityProjectRelations(moduleCode, moduleId, id, status, personId) {
      if (moduleCode == 3) {
        return this._http.post(this._commonService.baseUrl + '/fcoiDisclosure/project/relations', {
          'disclosureId': id,
          'proposalIdlinkedInDisclosure': moduleId,
          'disclosureStatusCode': status,
          'moduleCode': moduleCode,
          'moduleItemId': moduleId,
          'personId': personId,
        });
      } else {
        return this._http.post(this._commonService.baseUrl + '/fcoiDisclosure/project/relations', {
          'disclosureId': id,
          'disclosureStatusCode': status,
          'moduleCode': moduleCode,
          'moduleItemId': moduleId,
          'personId': personId
        });
      }
    }

    getSfiDetails(params: RO) {
        return this._http.post(this._commonService.baseUrl + '/personEntity/fetch', params);
    }

    updateProjectConflictStatus(params: any) {
        return this._http.post(this._commonService.baseUrl + '/updateProjectConflictStatus', params);
    }

    loadProjectConflictHistory(disclosureDetailsId: any) {
        return this._http.get(`${this._commonService.baseUrl}/loadProjectConflictHistory/${disclosureDetailsId}`);
    }

}
