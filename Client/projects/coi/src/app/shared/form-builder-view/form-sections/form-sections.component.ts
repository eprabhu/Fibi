import { FBConfiguration } from './../form-builder-interface';

import { EventEmitter, Input, OnChanges, Output, SimpleChanges, Component } from '@angular/core';
import { CustomElementVO, FormSection, QuestionnaireVO, SectionComponent } from '../form-builder-interface';
import { setCommentInput } from '../form-builder.service';

@Component({
    selector: 'app-form-sections',
    templateUrl: './form-sections.component.html',
    styleUrls: ['./form-sections.component.scss']
})
export class FormSectionsComponent {


    @Input() sectionDetails = new FormSection();
    @Input() saveEventForChildComponent;
    @Input() formBuilderId: number;
    @Input() fbConfiguration: FBConfiguration;
    @Input() isFormEditable;
    @Output()saveEventFromChildComponents = new EventEmitter<any>();
    @Output()emitCommentEvent = new EventEmitter<any>();

    constructor() { }

    saveEventsFromChild(data: QuestionnaireVO | CustomElementVO| any , component: SectionComponent) {
        const DATA = new SectionComponent();
        DATA.componentId = component.componentId;
        DATA.componentType = component.componentType;
        switch (component.componentType) {
            case 'QN': DATA.questionnaire = data.data; break;
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
            case 'TE':
            DATA.customElement = data.data; break;
            case 'PE': DATA.programmedElement = data.data; break;
        }
        this.saveEventFromChildComponents.emit(DATA);
    }

    setCommentDetails(headerName, sectionId, componentId) {
        this.emitCommentEvent.emit(setCommentInput(this.formBuilderId, sectionId, componentId, headerName));
    }

    canShowComments(component) {
        if (component.componentType == 'CE' && component.customElement && component.customElement.customDataElementId)
            return true;
        else if(component.componentType == 'QN' && component.questionnaire && component.questionnaire.header.QUESTIONNAIRE_ID)
            return true;
        else if(component.componentType == 'PE' && component.programmedElement && component.programmedElement.data.length)
            return true;
    }

}
