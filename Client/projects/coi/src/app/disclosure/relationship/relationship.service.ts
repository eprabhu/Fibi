import {HttpClient} from '@angular/common/http';
import {Injectable} from '@angular/core';
import {CommonService} from '../../common/services/common.service';

@Injectable()

export class RelationshipService {

    isSliderInputModified = false;
    isSliderDataUpdated = false;
    projectSFIDetails = {};


    constructor(private _http: HttpClient, private _commonService: CommonService) {
    }

    getProjectRelations(id, status) {
        return this._http.post(this._commonService.baseUrl + '/getDisclosureRelations', {
            disclosureId: id,
            disclosureStatusCode: status
        });
    }

    getEntityList(moduleCode, moduleId, id, status, personId) {
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

    getProjectsForEntity(disclosureId: number, personEntityId: number) {
        return this._http.post(this._commonService.baseUrl + '/disclosure/project/relations', {
            disclosureId,
            personEntityId,
        });
    }

    saveEntityProjectRelation(params, moduleCode, moduleItemId, disclosureId, personId) {
        return this._http.post(this._commonService.baseUrl + '/saveEntityProjectRelation',
            {
                'coiDisclEntProjDetails': params,
                'moduleCode': moduleCode,
                'moduleItemId': moduleItemId,
                'disclosureId': disclosureId,
                'personId': personId

            });
    }

    saveEntityProjectRelationSFI(params, personEntityId, disclosureId, personId) {
        return this._http.post(this._commonService.baseUrl + '/saveEntityProjectRelation',
            {
                coiDisclEntProjDetails: params, personEntityId, disclosureId, personId, isSfiProjectMapping: true
            });
    }

    singleEntityProjectRelation(params, moduleCode, moduleItemId, did, personId) {
        return this._http.post(this._commonService.baseUrl + '/singleEntityProjectRelation',
            {
                'coiDisclEntProjDetail': params,
                'moduleCode': moduleCode,
                'moduleItemId': moduleItemId,
                'disclosureId': did,
                'personId': personId
            });
    }

    singleEntityProjectRelationSFI(params, personEntityId, did, personId) {
        return this._http.post(this._commonService.baseUrl + '/singleEntityProjectRelation',
            {
                coiDisclEntProjDetail: params,
                disclosureId: did,
                personId: personId,
                isSfiProjectMapping: true
            });
    }

    updateConflictStatus(params) {
        return this._http.get(`${this._commonService.baseUrl}/validateConflicts/${params}`);
    }

    getSFIOfDisclosure(disclosureId: number) {
        return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', {
            'disclosureId': disclosureId,
            'reviewStatusCode': '',
            'filterType': 'FINANCIAL',
            'currentPage': 0,
            'pageNumber': 0,
            'personId': null,
            'searchWord': ''
        });
    }

}


