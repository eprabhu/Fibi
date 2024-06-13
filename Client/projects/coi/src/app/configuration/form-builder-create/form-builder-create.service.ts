import { Injectable } from '@angular/core';
import { Observable, Subject } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
import { FormBuilderEvent, FormSection } from '../../shared/form-builder-view/form-builder-interface';
import {
    ComponentObjects, UpdateFormUsage, saveFormUsage, UpdateSectionObject, UpdateFormHeaderObject, ComponentOrder, CreateComponentObject, CreateFormHeader,
    FormSectionObject, SectionOrder,
    configureCustomElement
} from './form-builder-create-interface';

@Injectable()
export class FormBuilderCreateService {

    unSavedChange = false;
    autoSaveTrigger$: Subject<any> = new Subject<any>();
    formBuilderEvents = new Subject<FormBuilderEvent>();
    formEditorState: Array<FormSection> = [];
    selectedComponent;
    currentComponentPosition = {
        tempId: "",
        sectionId: "",
        orderNo: ""
    }
    newComponentPosition = {
        sectionId: "",
        orderNo: ""
    }

    constructor(private _commonService: CommonService, private _http: HttpClient) { }

    initiateAutoSave(saveEvent: string) {
        // this.autoSaveTrigger$.next(saveEvent);
    }

    isComponentOrderChange(): boolean {
        for (let ele of this.formEditorState) {
            this.selectedComponent = ele.sectionComponent.find(ele => ele.tempId == this.currentComponentPosition.tempId)
            if (this.selectedComponent && this.selectedComponent.sectionId == this.currentComponentPosition.sectionId && this.selectedComponent.componentOrderNumber == this.currentComponentPosition.orderNo) {
                return false;
            }
            else if (this.selectedComponent) {
                this.newComponentPosition.sectionId = this.selectedComponent.sectionId;
                this.newComponentPosition.orderNo = this.selectedComponent.componentOrderNumber;
                return true;

            }

        }
    }

    isEmptySectionPresent(): boolean {
        let emptySection;
        emptySection = this.formEditorState.find(x => x.sectionComponent?.length == 0)
        if (emptySection) {
            return true;
        }
        return false;
    }

    isUnconfiguredcomponentsPresent(): boolean {
        let unconfiguredComponent;
        for (let ele of this.formEditorState) {
            unconfiguredComponent = ele.sectionComponent.find(x => x.tempId)
            if (unconfiguredComponent) {
                return true;
            }
        }
        return false;
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

    publishForm(formDetails: CreateFormHeader): Observable<any> {
        return this._http.put(this._commonService.baseUrl + "/formbuilder/config/v1/formheader", formDetails);
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

    updateComponent(formComponent: ComponentObjects): Observable<any> {
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
    getSystemLookupByCustomType(dataTypeCode:{dataTypeCode:string}): Observable<any> {
        return this._http.post(this._commonService.baseUrl + "/formbuilder/config/v1/getSystemLookupByCustomType", dataTypeCode);
    }

    configureCustomElement(customData: configureCustomElement): Observable<any> {
        return this._http.post(this._commonService.baseUrl + "/formbuilder/config/v1/configureCustomElement", customData);
    }

    fetchCustomData(customDataId:{customDataElementId:string}): Observable<any> {
        return this._http.post(this._commonService.baseUrl + "/formbuilder/fetchFormCustomElementById", customDataId);
    }

    getModuleList(): Observable<any> {
        return this._http.get(this._commonService.baseUrl + "/getModuleList");
    }

    saveFormUsage(integationObj: saveFormUsage): Observable<any> {
        return this._http.post(this._commonService.baseUrl + "/formbuilder/config/v1/formusage", integationObj);
    }

    updateFormUsage(integationObj: UpdateFormUsage): Observable<any> {
        return this._http.put(this._commonService.baseUrl + "/formbuilder/config/v1/formusage", integationObj);
    }

    getAllFormUsage(formBuilderId: string): Observable<any> {
        return this._http.get(this._commonService.baseUrl + `/formbuilder/config/v1/formusage/${formBuilderId}`);
    }

    deleteusage(usageID: number): Observable<any> {
        return this._http.delete(this._commonService.baseUrl + `/formbuilder/config/v1/formusage/${usageID}`);
    }

}

