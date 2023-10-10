
import { Component, EventEmitter, Input, OnChanges, OnInit, Output, SimpleChanges } from '@angular/core';
import { CustomElementVO, FormBuilder, FormBuilderEvent, FormBuilderSaveRO, QuestionnaireVO,
        FBConfiguration } from './form-builder-interface';
import { Observable, Subject } from 'rxjs';
import { FormBuilderService } from './form-builder.service';

@Component({
    selector: 'app-form-builder-view',
    templateUrl: './form-builder-view.component.html',
    styleUrls: ['./form-builder-view.component.scss'],
    providers: [FormBuilderService]
})
export class FormBuilderViewComponent implements OnInit, OnChanges {

    @Input() externalEvents: Observable<FormBuilderEvent>;
    @Output() builderStatus = new EventEmitter<string>();
    formBuilderData = new FormBuilder();
    isSubscribed = false;
    saveEventForChildComponent = new Subject();
    fbConfiguration = new FBConfiguration();

    constructor( private  _formBuilderService: FormBuilderService) {}

    ngOnChanges(changes: SimpleChanges): void {
        this.subscribeToExternalEvents();
        this.isSubscribed = true;
    }

    ngOnInit(): void {
        this.builderStatus.emit('READY');
    }

    private subscribeToExternalEvents(): void {
        if (!this.isSubscribed) {
            this.externalEvents.subscribe((E: FormBuilderEvent) => {
                if (E.eventType === 'CONFIGURATION') {
                    this.fbConfiguration = E.data;
                    this.getFormBuilderData();
                }
                if (E.eventType === 'SAVE') {
                    this.saveEventForChildComponent.next({saveMethod: 'EXTERNAL'});
                }
            });
        }
    }

    private getFormBuilderData(): void {
        this._formBuilderService.getFormBuilderData().subscribe((data: any) => {
            this.formBuilderData = data;
        });
    }

    saveEventFromChildComponents(data: CustomElementVO | QuestionnaireVO| any ) {
        const RO: FormBuilderSaveRO = this.prepareROForSave(data);
        console.log(RO);
        this._formBuilderService.saveFormComponent(RO).subscribe(data => {
            console.log('done');
        }, err => console.log('ooppzzz'));
    }

    prepareROForSave(data: CustomElementVO | QuestionnaireVO| any ): FormBuilderSaveRO {
        const RO = new FormBuilderSaveRO();
        RO.formId = this.formBuilderData.form.formBuilderId;
        RO.moduleItemCode = this.fbConfiguration.moduleItemCode;
        RO.moduleItemKey = this.fbConfiguration.moduleItemKey;
        RO.moduleSubItemCode = this.fbConfiguration.moduleSubItemCode;
        RO.moduleSubItemKey = this.fbConfiguration.moduleSubItemKey;
        RO.documentOwnerPersonId = this.fbConfiguration.documentOwnerPersonId;
        RO.componentId = data.componentId;
        RO.componentType = data.componentType;
        switch (data.componentType) {
            case 'QN': RO.questionnaire = data.questionnaire;
                        RO.files = data.questionnaire.files; break;
            case 'CE': RO.customElement = data.customElement; break;
            case 'PE': RO.programmedElement = data.programmedElement; break;
        }
        return RO;
    }


}
