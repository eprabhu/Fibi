import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../../common/services/common.service';
import { GetSFIRequestObject } from '../../disclosure/coi-interface';

@Injectable()
export class CountModalService {

    constructor(private _http: HttpClient,
                private _commonService: CommonService) {
    }

    getAwardProposalSFIList(id) {
        return this._http.post(this._commonService.baseUrl + '/getDisclosureRelationForSFI', {'coiFinancialEntityId': id});
    }

    getSFICount(params: GetSFIRequestObject) {
        return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', params);
    }

    getProjectsCount(id, disclosureSequenceStatusCode, personId) {
        return this._http.post(this._commonService.baseUrl + '/getDisclosureRelations',
        {'disclosureId': id, 'disclosureStatusCode': disclosureSequenceStatusCode,
        'personId': personId});
    }

    getDisclosureDetails(id) {
        return this._http.get(`${this._commonService.baseUrl}/getDisclosureDetailsForSFI/${id}`);
    }
}
