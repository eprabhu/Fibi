import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { Subject } from 'rxjs';
import { FormBuilderEvent } from '../../shared/form-builder-view/form-builder-interface';

@Injectable()

export class ConsultingService {

  previousHomeUrl = '';
  globalSave$: Subject<any> = new Subject<any>();
  formBuilderEvents = new Subject<FormBuilderEvent>();
  isFormBuilderDataChangePresent = false;
  triggerSaveComplete = new Subject<boolean>();
  concurrentUpdateAction : string;
  $triggerFormValidate: Subject<any> = new Subject<any>();
  isDataChangeAvailableInEntity = false;
  canDisableSubmit = true;
  headerEntityName = '';

  constructor(private _http: HttpClient,
    private _commonService: CommonService) { }

    loadConsultingFormHeader(disclosureId) {
        return this._http.get(this._commonService.consultingUrl + '/getDisclosureHeader/' + disclosureId);
    }

    submitConsulting(disclosureId) {
        return this._http.patch(`${this._commonService.consultingUrl}/submit`, {disclosureId});
    }

    withdrawConsulting(params) {
        return this._http.patch(`${this._commonService.consultingUrl}/withdraw`, params);
    }

    returnConsulting(params) {
        return this._http.patch(`${this._commonService.consultingUrl}/return`, params);
    }

    completeFinalReview(disclosureId) {
        return this._http.patch(`${this._commonService.consultingUrl}/complete/${disclosureId}`, {});
    }

    disclosureHistory(disclosureId) {
        return this._http.get(`${this._commonService.consultingUrl}/history/${disclosureId}`);
    }

    validateForm(configuration: any) {
        return this._http.post(this._commonService.formUrl + '/formbuilder/validateForm', configuration);
    }

}
