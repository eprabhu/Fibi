import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { EntityRiskCategoryCode, EntityRiskProxyController, EntityRiskRO } from '../entity-interface';

@Injectable()
export class EntityRiskSectionService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    saveEntityRisk(entityRiskRO: EntityRiskRO, proxyController: EntityRiskProxyController) {
        return this._http.post(`${this._commonService.baseUrl}/entity${proxyController}/saveRisk`, entityRiskRO);
    }

    deleteEntityRisk(entityRiskId: number, proxyController: EntityRiskProxyController) {
        return this._http.get(`${this._commonService.baseUrl}/entity${proxyController}/deleteRisk/${entityRiskId}`);
    }

    updateEntityRisk(entityRiskRO: EntityRiskRO, proxyController: EntityRiskProxyController) {
        return this._http.patch(`${this._commonService.baseUrl}/entity${proxyController}/updateRisk`, entityRiskRO, { responseType: 'text' });
    }

    fetchRiskTypes(riskcategoryCode: EntityRiskCategoryCode) {
        return this._http.get(`${this._commonService.baseUrl}/entity/fetchRiskTypes/${riskcategoryCode}`);
    }

    fetchRiskLevels(sectionCode: string) {
        return this._http.get(`${this._commonService.baseUrl}/entity/fetchRiskLevels/${sectionCode}`);
    }
}
