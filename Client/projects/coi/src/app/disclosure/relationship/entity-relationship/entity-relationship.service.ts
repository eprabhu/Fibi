import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { RO } from '../../coi-interface';

class ProjectDetails {
	projectNumber = '';
	projectName = '';
}
@Injectable()
export class EntityRelationshipService {

$isProjectListVisible = new Subject();
$setSelectedProjectDetails = new Subject();
projectDetails = new Subject();
$previousNext = new Subject();
$coiStatus = new Subject();
$updateSFIComplete = new Subject();
$openModalDetails = new Subject();

currentCommonProjectArray: any;
commonProjectArray: any = [];
test = new Subject();

currentProject: ProjectDetails = new ProjectDetails();
nextProject: ProjectDetails = new ProjectDetails();
nextProjectPosition: any;
nextProjectModuleItemKey: any;
currentProjectModuleItemKey: any;

linkedProposalId: any;

constructor(
  private _http: HttpClient,
  private _commonService: CommonService
) { }

getProjectRelationships(id, disclosureStatusCode, personId) {
  return this._http.post(this._commonService.baseUrl + '/getDisclosureRelations',
  // tslint:disable-next-line: radix
  {'disclosureId': parseInt(id), 'disclosureStatusCode': disclosureStatusCode,
    'personId': personId});
}

getSfiDetails(params: RO) {
  return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', params);
}

saveEntityProjectRelation(params, moduleCode, moduleItemId, did, personId) {
  return this._http.post(this._commonService.baseUrl + '/saveEntityProjectRelation',
  {'coiDisclosureDetails': params,
  'moduleCode': moduleCode,
  'moduleItemId': moduleItemId,
   'disclosureId': did,
   'personId': personId
});
}

getEntityProjectRelations(moduleCode, moduleItemId, disclosureId, disclosureStatusCode, personId, isProposalDisclosure) {
  if (!isProposalDisclosure) {
      return this._http.post(this._commonService.baseUrl + '/disclosure/project/relations',
    {
    'moduleCode': moduleCode,
    'moduleItemId': moduleItemId,
    // tslint:disable-next-line: radix
    'disclosureId': parseInt(disclosureId),
    'disclosureStatusCode': disclosureStatusCode,
    'personId': personId
    });
  } else {
    return this._http.post(this._commonService.baseUrl + '/disclosure/project/relations',
  {
  'moduleCode': moduleCode,
  'moduleItemId': moduleItemId,
  // tslint:disable-next-line: radix
  'disclosureId': parseInt(disclosureId),
  'disclosureStatusCode': disclosureStatusCode,
  'personId': personId,
  'proposalIdlinkedInDisclosure': moduleItemId
  });
  }
}

}
