import { Questionnaire } from './../../view-questionnaire-v2/questionnaire-interface';
import { Component, EventEmitter, Input, Output, } from '@angular/core';
import { CustomElementVO, FBConfiguration, FormBuilderSaveRO, FormSection, QuestionnaireVO, SectionComponent } from '../form-builder-interface';
import { FormBuilderService } from '../form-builder.service';
import { CommonService } from '../../../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../../app-constants';

@Component({
    selector: 'app-data-layer',
    templateUrl: './data-layer.component.html',
    styleUrls: ['./data-layer.component.scss']
})
export class DataLayerComponent {

    @Input() component: SectionComponent;
    @Input() fbConfiguration: FBConfiguration;
    @Input() saveEventForChildComponent;
    @Input() isFormEditable: boolean;
    @Input() formBuilderId: number;

    constructor(private _formBuilderService: FormBuilderService, private _commonService: CommonService) { }

    saveEventsFromChild(data: CustomElementVO | QuestionnaireVO | any) {
        const RO: FormBuilderSaveRO = this.prepareROForSave(data);
        this._formBuilderService.saveFormComponent(RO).subscribe((res: SectionComponent) => {
            if (this.component.componentType === 'QN') {
                this.component.questionnaire = res.questionnaire;
            } else if (['SE','NE','DE','CB','RB','ES','AS','SD','UD','TE','CE'].includes(this.component.componentType)) {
                this.component.customElement.customDataElements = res.customElement.customDataElements;
            }
            this.saveEventForChildComponent.next({ eventType: 'SAVE_COMPLETE', data: res });
            this.emitEditOrSaveAction('SAVE_COMPLETE', res);
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Form saved successfully.');
        }, err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving Form. Please try again.');
        });
    }

    prepareROForSave(data: CustomElementVO | QuestionnaireVO | any): FormBuilderSaveRO {
        const RO = new FormBuilderSaveRO();
        RO.formBuilderId = this.formBuilderId;
        RO.moduleItemCode = this.fbConfiguration.moduleItemCode;
        RO.moduleItemKey = this.fbConfiguration.moduleItemKey;
        RO.moduleSubItemCode = this.fbConfiguration.moduleSubItemCode;
        RO.moduleSubItemKey = this.fbConfiguration.moduleSubItemKey;
        RO.documentOwnerPersonId = this.fbConfiguration.documentOwnerPersonId;
        RO.componentId = this.component.componentId;
        RO.componentType = this.component.componentType;
        switch (this.component.componentType) {
            case 'QN': RO.questionnaire = data.data;
                RO.files = data.files;
                delete RO.questionnaire.files; break;
            case 'CE':
            case 'SE': 
            case 'NE': 
            case 'DE': 
            case 'CB': 
            case 'RB': 
            case 'ES': 
            case 'AS': 
            case 'SD': 
            case 'UD': 
            case 'TE':  RO.customElement = data.data; break;
            case 'PE': RO.programmedElement = data.data; break;
        }
        return RO;
    }

    emitEditOrSaveAction(actionPerformed, event) {
        this._formBuilderService.$formBuilderActionEvents.next({action: actionPerformed, actionResponse: event, component: this.component});
    }

}
