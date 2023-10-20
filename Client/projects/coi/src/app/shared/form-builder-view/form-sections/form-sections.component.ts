import { FBConfiguration } from './../form-builder-interface';

import { EventEmitter, Input, OnChanges, Output, SimpleChanges, Component } from '@angular/core';
import { CustomElementVO, FormSection, QuestionnaireVO, SectionComponent } from '../form-builder-interface';

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

    constructor() { }

    saveEventsFromChild(data: QuestionnaireVO | CustomElementVO| any , component: SectionComponent) {
        const DATA = new SectionComponent();
        DATA.componentId = component.componentId;
        DATA.componentType = component.componentType;
        switch (component.componentType) {
            case 'QN': DATA.questionnaire = data.data; break;
            case 'CE': DATA.customElement = data.data; break;
            case 'PE': DATA.programmedElement = data.data; break;
        }
        this.saveEventFromChildComponents.emit(DATA);
    }

}
