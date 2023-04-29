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

getEntityList(moduleCode, moduleId, id, status) {
  if (moduleCode == 3) {
    return this._http.post(this._commonService.baseUrl + '/getEntityProjectRelations', {
      'disclosureId': id,
      'proposalIdlinkedInDisclosure': moduleId,
      'disclosureStatusCode': status,
      'moduleCode': moduleCode,
      'moduleItemId': moduleId,
      'personId': "10000000001",
    });
  } else {
    return this._http.post(this._commonService.baseUrl + '/getEntityProjectRelations', {
      'disclosureId': id,
      'disclosureStatusCode': status,
      'moduleCode': moduleCode,
      'moduleItemId': moduleId,
      'personId': "10000000001"
    });
  }
}

saveEntityProjectRelation(params, moduleCode, moduleItemId, did) {
  return this._http.post(this._commonService.baseUrl + '/saveEntityProjectRelation',
  {'coiDisclEntProjDetails': params,
  'moduleCode': moduleCode,
  'moduleItemId': moduleItemId,
   'disclosureId': did,
});
}

singleEntityProjectRelation(params, moduleCode, moduleItemId, did) {
  return this._http.post(this._commonService.baseUrl + '/singleEntityProjectRelation',
  {'coiDisclEntProjDetail': params,
  'moduleCode': moduleCode,
  'moduleItemId': moduleItemId,
   'disclosureId': did,});
}

}


