import { Injectable } from '@angular/core';
import { Observable, Subject, of } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
import { FBActionEvent, FBConfiguration, FormBuilderSaveRO } from './form-builder-interface';

@Injectable()

export class FormBuilderService {

    baseURL = '';
    $formBuilderActionEvents = new Subject<FBActionEvent>();

    constructor(private _http: HttpClient, private _commonService: CommonService) {
        this.baseURL = this._commonService.formUrl;
     }

    getFormBuilderData(configuration: FBConfiguration) {
        return this._http.post(this.baseURL + '/formbuilder/getForm', configuration);
    }

    getOpaPersonType(): any {
        return this._http.get(this.baseURL + '/opa' + '/getOpaPersonType');
    }
    getFormBuilderDataForBlankForm(configuration: FBConfiguration) {
        return this._http.post(this.baseURL + '/formbuilder/getBlankForm', configuration);
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
        return this._http.post(this.baseURL + '/formbuilder/saveFormComponent', formData);
    }

}

export function setCommentInput(formBuilderId, formBuilderSectionId, formBuilderComponentId, headerName): any{
    const COMMENT_META_DATA: any = {
        "componentTypeCode": '10',
        "formBuilderId": formBuilderId,
        "formBuilderSectionId": formBuilderSectionId,
        "formBuilderComponentId": formBuilderComponentId,
        "headerName": headerName
    }
    return COMMENT_META_DATA;
}



