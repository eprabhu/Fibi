import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()

export class RelationshipService {

constructor(private _http: HttpClient, private _commonService: CommonService) { }

getProjectRelations(id, status) {
  return this._http.post(this._commonService.baseUrl + '/getDisclosureRelations', {
    disclosureId : id,
    disclosureStatusCode: status
  });
}

getEntityList(moduleCode, moduleId, id, status,personId) {
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

saveEntityProjectRelation(params, moduleCode, moduleItemId, disclosureId,personId) {
  return this._http.post(this._commonService.baseUrl + '/saveEntityProjectRelation',
  {'coiDisclEntProjDetails': params,
  'moduleCode': moduleCode,
  'moduleItemId': moduleItemId,
  'disclosureId': disclosureId,
  'personId': personId

});
}

singleEntityProjectRelation(params, moduleCode, moduleItemId, did,personId) {
  return this._http.post(this._commonService.baseUrl + '/singleEntityProjectRelation',
  {'coiDisclEntProjDetail': params,
  'moduleCode': moduleCode,
  'moduleItemId': moduleItemId,
   'disclosureId': did,
   'personId': personId
  });
}

updateConflictStatus(params) {
  return this._http.get(`${this._commonService.baseUrl}/validateConflicts/${params}`);
}

}


