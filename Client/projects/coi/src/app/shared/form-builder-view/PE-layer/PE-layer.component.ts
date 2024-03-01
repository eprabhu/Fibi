import { Component, Input, OnChanges, OnInit, SimpleChanges, ViewChild } from '@angular/core';
import { HostContainerDirective } from '../../directives/host-container.directive';
import { CustomElementVO, FBConfiguration, FormBuilderSaveRO, QuestionnaireVO, SectionComponent } from '../form-builder-interface';
import { OPACompUncompComponent } from '../PE-components/OPA-comp-uncomp/OPA-comp-uncomp.component';
import { OPAOutsideFinancialRelationComponent } from '../PE-components/OPA-outside-financial-relation/OPA-outside-financial-relation.component';
import { FormBuilderService } from '../form-builder.service';
import { Subject } from 'rxjs';
import { OPAInstituteResourceUseComponent } from '../PE-components/OPA-institute-resources/OPA-institute-resources.component';
import { OPAStudentSubordinateEmployeeComponent } from '../PE-components/OPA-student-subordinate-employee/OPA-student-subordinate-employee.component';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';

@Component({
    selector: 'app-PE-layer',
    template: '<ng-template appHostContainer></ng-template>',
    styleUrls: ['./PE-layer.component.scss']
})
export class PELayerComponent implements OnInit, OnChanges {

    @ViewChild(HostContainerDirective, { static: true }) adHost!: HostContainerDirective;
    @Input() component: SectionComponent;
    @Input() fbConfiguration: FBConfiguration;
    @Input() isFormEditable: boolean;
    @Input() formBuilderId: number;
    @Input() sectionName = '';
    saveEventForChildComponent = new Subject<any>();

    constructor(private _formBuilder: FormBuilderService, private _commonService: CommonService) { }

    ngOnInit() {
    }

    ngOnChanges(changes: SimpleChanges) {
        this.loadComponent();
    }

    loadComponent() {
        const viewContainerRef = this.adHost.viewContainerRef;
        viewContainerRef.clear();
        switch (this.component.componentData) {
            case 'OPACompUncompComponent': this.loadOPACompUncompComponent(viewContainerRef); break;
            case 'OPAOutsideFinancialRelationComponent': this.loadOPAOutsideFinancialRelationComponent(viewContainerRef); break;
            case 'OPAInstituteResourceUseComponent': this.loadOPAInstituteResourceUseComponent(viewContainerRef); break;
            case 'OPAStudentSubordinateInvolvementComponent': this.loadOPAStudentSubordinateEmployeeComponent(viewContainerRef); break;
        }
    }

    loadOPACompUncompComponent(viewContainerRef) {
        const componentRef = viewContainerRef.createComponent(OPACompUncompComponent);
        componentRef.instance.componentData = this.component.programmedElement;
        componentRef.instance.formBuilderId = this.fbConfiguration.moduleItemKey;
        componentRef.instance.externalEvents = this.saveEventForChildComponent;
        componentRef.instance.isFormEditable = this.isFormEditable;
        componentRef.instance.sectionHeading = this.sectionName;
        componentRef.instance.childEvents.subscribe( event => this.saveEventsFromChild(event));
    }

    loadOPAOutsideFinancialRelationComponent(viewContainerRef) {
        const componentRef = viewContainerRef.createComponent(OPAOutsideFinancialRelationComponent);
        componentRef.instance.componentData = this.component.programmedElement;
        componentRef.instance.formBuilderId = this.fbConfiguration.moduleItemKey;
        componentRef.instance.isFormEditable = this.isFormEditable;
        componentRef.instance.externalEvents = this.saveEventForChildComponent;
        componentRef.instance.sectionHeading = this.sectionName;
        componentRef.instance.childEvents.subscribe( event => this.saveEventsFromChild(event));
    }

    loadOPAInstituteResourceUseComponent(viewContainerRef) {
        const componentRef = viewContainerRef.createComponent(OPAInstituteResourceUseComponent);
        componentRef.instance.componentData = this.component.programmedElement;
        componentRef.instance.formBuilderId = this.fbConfiguration.moduleItemKey;
        componentRef.instance.isFormEditable = this.isFormEditable;
        componentRef.instance.externalEvents = this.saveEventForChildComponent;
        componentRef.instance.sectionHeading = this.sectionName;
        componentRef.instance.childEvents.subscribe( event => this.saveEventsFromChild(event));
    }

    loadOPAStudentSubordinateEmployeeComponent(viewContainerRef) {
        const COMPONENT_REF = viewContainerRef.createComponent(OPAStudentSubordinateEmployeeComponent);
        COMPONENT_REF.instance.componentData = this.component.programmedElement;
        COMPONENT_REF.instance.formBuilderId = this.fbConfiguration.moduleItemKey;
        COMPONENT_REF.instance.isFormEditable = this.isFormEditable;
        COMPONENT_REF.instance.externalEvents = this.saveEventForChildComponent;
        COMPONENT_REF.instance.childEvents.subscribe( event => this.saveEventsFromChild(event));
    }

    saveEventsFromChild(data: CustomElementVO | QuestionnaireVO | any) {
        const RO: FormBuilderSaveRO = this.prepareROForSave(data);
        this._formBuilder.saveFormComponent(RO).subscribe((res: SectionComponent) => {
            this.saveEventForChildComponent.next({ eventType: 'SAVE_COMPLETE', data: res.programmedElement });
            switch (data.action) {
                case 'ADD': this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Entity added successfully.'); break;
                case 'UPDATE': this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Entity updated successfully.'); break;
                case 'DELETE': this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Entity removed successfully.'); break;
                default: break;
            }
        }, err => {
            switch (data.action) {
                case 'ADD': this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in adding entity. Please try again.'); break;
                case 'UPDATE': this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating entity. Please try again.'); break;
                case 'DELETE': this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in deleting entity. Please try again.'); break;
                default: break;
            }
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
        RO.componentData = this.component.componentData;
        RO.componentRefId = this.component.componentRefId;
        RO.programmedElement = data.data;
        return RO;
    }
}
