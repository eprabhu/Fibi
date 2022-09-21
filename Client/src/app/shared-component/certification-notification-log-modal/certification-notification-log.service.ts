import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
import { CertificationLogRO, PersonNotifyMailRO } from '../../proposal/interface/proposal.interface';

@Injectable()
export class CertificationNotificationLogService {

    constructor(private _http: HttpClient, private _commonService: CommonService) {
    }

    getPersonCertificationMailLog(params: CertificationLogRO) {
        return this._http.post(`${this._commonService.baseUrl}/personCertificationMailLog`, params);
    }

    sendPersonCertificationMail(params: PersonNotifyMailRO) {
        return this._http.post(`${this._commonService.baseUrl}/sendPersonCertificationMail`, params);
    }

    getProposalPersonsForCertification(proposalId: number) {
        return this._http.get(`${this._commonService.baseUrl}/proposalPersonsForCertification/${proposalId}`);
    }
}
