import { Questionnaire } from './../../view-questionnaire-v2/questionnaire-interface';
import { Component, Input, } from '@angular/core';
import { CustomElementVO, FBConfiguration, FormBuilderSaveRO, FormSection, QuestionnaireVO, SectionComponent } from '../form-builder-interface';
import { FormBuilderService } from '../form-builder.service';

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

    constructor(private _formBuilderService: FormBuilderService) { }

    saveEventsFromChild(data: CustomElementVO | QuestionnaireVO | any) {
        const RO: FormBuilderSaveRO = this.prepareROForSave(data);
        this._formBuilderService.saveFormComponent(RO).subscribe((res: SectionComponent) => {
            if (this.component.componentType === 'QN') {
                this.component.questionnaire.questionnaire = res.questionnaire.questionnaire;
            } else if (this.component.componentType === 'CE') {
                this.component.customElement.customDataElements = res.customElement.customDataElements;
            }
            this.saveEventForChildComponent.next({ eventType: 'SAVE_COMPLETE', data: res });
        }, err => {
            // Do something
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
            case 'CE': RO.customElement = data.data; break;
            case 'PE': RO.programmedElement = data.data; break;
        }
        return RO;
    }


}
