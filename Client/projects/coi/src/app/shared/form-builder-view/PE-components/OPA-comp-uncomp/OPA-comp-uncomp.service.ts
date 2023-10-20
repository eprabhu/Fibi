import { Injectable } from '@angular/core';
import { FormBuilderService } from '../../form-builder.service';
import { RelationShipSaveRO, EntitySaveRO } from './interface';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class OPACompUncompService {

constructor( private _fbService: FormBuilderService, private _http: HttpClient) { }

  saveEntityOrRelation(RO: EntitySaveRO| RelationShipSaveRO ): Promise<any> {
   return RO.hasOwnProperty('entityId') ? this.saveEntity(RO) : this.saveEntityRelationOnly(RO);
  }

  saveEntity(RO: EntitySaveRO| RelationShipSaveRO ): Promise<any> {
    return this._http.post(this._fbService.baseURL + '/createSFI', RO).toPromise();
  }

  saveEntityRelationOnly(RO: EntitySaveRO| RelationShipSaveRO ): Promise<any> {
    return this._http.post(this._fbService.baseURL + '/saveOrUpdateCoiFinancialEntityDetails', RO).toPromise();
  }
}
