import { Injectable } from '@angular/core';
import { Observable, of } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
import { FBConfiguration, FormBuilderSaveRO } from './form-builder-interface';

@Injectable()

export class FormBuilderService {

    baseURL = '';
    constructor(private _http: HttpClient, private _commonService: CommonService) {
        this.baseURL = this._commonService.baseUrl;
     }

    getFormBuilderData(configuration: FBConfiguration) {
        return this._http.post(this._commonService.baseUrl + '/formbuilder/getForm', configuration);
    }

    saveFormComponent(data: FormBuilderSaveRO): Observable<any> {
        const formData = new FormData();
        formData.append('formBuilderId', data.formBuilderId.toString());
        formData.append('documentOwnerPersonId', data.documentOwnerPersonId);
        formData.append('moduleItemCode', data.moduleItemCode);
        formData.append('moduleSubItemCode', data.moduleSubItemCode);
        formData.append('moduleItemKey', data.moduleItemKey);
        formData.append('moduleSubItemKey', data.moduleSubItemKey);
        formData.append('componentId', data.componentId.toString());
        formData.append('componentType', data.componentType);
        formData.append('programmedElement', JSON.stringify(data.programmedElement));
        formData.append('questionnaire', JSON.stringify(data.questionnaire));
        formData.append('customElement', JSON.stringify(data.customElement));
        if (data.componentType === 'PE') {
            formData.append('componentRefId', data.componentRefId);
            formData.append('componentData', data.componentData);
        }
        if (data?.files?.length > 0) {
            data.files.forEach(file => {
                formData.append(file.questionId + '', file.attachment, file.attachment.name);
            });
        }
        return this._http.post(this._commonService.baseUrl + '/formbuilder/saveFormComponent', formData);
    }

}

