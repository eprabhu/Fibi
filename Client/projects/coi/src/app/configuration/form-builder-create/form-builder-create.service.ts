import { Injectable } from '@angular/core';
import { Observable, Subject } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
import { ComponentObject, ComponentOrder, CreateComponentObject, CreateFormHeader, FormSectionObject, SectionOrder, UpdateFormHeaderObject, UpdateSectionObject } from '../../shared/form-builder-view/form-builder-interface';

@Injectable()
export class FormBuilderCreateService {

    unSavedChange = false;
    autoSaveTrigger$: Subject<any> = new Subject<any>();

    constructor(private _commonService: CommonService, private _http: HttpClient) { }

    initiateAutoSave(saveEvent: string) {
        this.autoSaveTrigger$.next(saveEvent);
    }

    getFormList(): Observable<any> {
        return this._http.get(this._commonService.baseUrl + '/formbuilder/config/v1/formlist');
    }

    getFormDeatails(formBuilderId: string): Observable<any> {
        return this._http.get(this._commonService.baseUrl + `/formbuilder/config/v1/form/${formBuilderId}`);
    }

    createFormHeader(formDetails: CreateFormHeader): Observable<any> {
        return this._http.post(this._commonService.baseUrl + "/formbuilder/config/v1/formheader", formDetails);
    }

    createFormSection(formSection: FormSectionObject): Observable<any> {
        return this._http.post(this._commonService.baseUrl + "/formbuilder/config/v1/formsection", formSection);
    }

    createComponent(formComponent: CreateComponentObject): Observable<any> {
        return this._http.post(this._commonService.baseUrl + "/formbuilder/config/v1/sectioncomponent", formComponent);
    }

    deleteComponent(componentId: number): Observable<any> {
        return this._http.delete(this._commonService.baseUrl + `/formbuilder/config/v1/sectioncomponent/${componentId}`);
    }

    componentOrder(formOrder: Array<ComponentOrder>): Observable<any> {
        return this._http.patch(this._commonService.baseUrl + "/formbuilder/config/v1/sectioncomponent/order", formOrder);
    }

    updateComponent(formComponent: ComponentObject): Observable<any> {
        return this._http.put(this._commonService.baseUrl + "/formbuilder/config/v1/sectioncomponent", formComponent);
    }

    getProgramElementList(): Observable<any> {
        return this._http.get(this._commonService.baseUrl + "/formbuilder/config/v1/programmedElementList");
    }

    getQuestionnaireList(): Observable<any> {
        return this._http.get(this._commonService.baseUrl + "/formbuilder/config/v1/questionnaireList");
    }

    getCustomElementList(): Observable<any> {
        return this._http.get(this._commonService.baseUrl + "/formbuilder/config/v1/customElementList");
    }

    readComponent(formBuilderSectCompId: number): Observable<any> {
        return this._http.get(this._commonService.baseUrl + `/formbuilder/config/v1/sectioncomponent/${formBuilderSectCompId}`);
    }

    sectionOrder(sectionOrder: Array<SectionOrder>): Observable<any> {
        return this._http.patch(this._commonService.baseUrl + "/formbuilder/config/v1/formsection/order", sectionOrder);
    }

    updateSection(formSection: UpdateSectionObject): Observable<any> {
        return this._http.put(this._commonService.baseUrl + "/formbuilder/config/v1/formsection", formSection);
    }

    readSection(formbuilderSectionId: number): Observable<any> {
        return this._http.get(this._commonService.baseUrl + `/formbuilder/config/v1/formsection/${formbuilderSectionId}`);
    }

    deleteSection(sectionId: number): Observable<any> {
        return this._http.delete(this._commonService.baseUrl + `/formbuilder/config/v1/formsection/${sectionId}`);
    }

    updateFormHeader(formDetails: UpdateFormHeaderObject): Observable<any> {
        return this._http.put(this._commonService.baseUrl + "/formbuilder/config/v1/formheader", formDetails);
    }

}

