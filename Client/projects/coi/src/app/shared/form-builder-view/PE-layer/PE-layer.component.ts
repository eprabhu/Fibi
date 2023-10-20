import { Component, Input, OnInit, ViewChild } from '@angular/core';
import { HostContainerDirective } from '../../directives/host-container.directive';
import { CustomElementVO, FBConfiguration, FormBuilderSaveRO, QuestionnaireVO, SectionComponent } from '../form-builder-interface';
import { OPACompUncompComponent } from '../PE-components/OPA-comp-uncomp/OPA-comp-uncomp.component';
import { FormBuilderService } from '../form-builder.service';
import { Subject } from 'rxjs';

@Component({
    selector: 'app-PE-layer',
    template: '<ng-template appHostContainer></ng-template>',
    styleUrls: ['./PE-layer.component.scss']
})
export class PELayerComponent implements OnInit {

    @ViewChild(HostContainerDirective, { static: true }) adHost!: HostContainerDirective;
    @Input() component: SectionComponent;
    @Input() fbConfiguration: FBConfiguration;
    @Input() isFormEditable: boolean;
    @Input() formBuilderId: number;
    saveEventForChildComponent = new Subject<any>();

    constructor(private _formBuilder: FormBuilderService) { }

    ngOnInit() {
        this.loadComponent();
    }

    loadComponent() {
        const viewContainerRef = this.adHost.viewContainerRef;
        viewContainerRef.clear();
        const componentRef = viewContainerRef.createComponent(OPACompUncompComponent);
        componentRef.instance.componentData = this.component.programmedElement;
        componentRef.instance.formBuilderId = this.fbConfiguration.moduleItemKey;
        componentRef.instance.externalEvents = this.saveEventForChildComponent;
        componentRef.instance.childEvents.subscribe( event => this.saveEventsFromChild(event));
    }

    saveEventsFromChild(data: CustomElementVO | QuestionnaireVO | any) {
        const RO: FormBuilderSaveRO = this.prepareROForSave(data);
        this._formBuilder.saveFormComponent(RO).subscribe((res: SectionComponent) => {
            this.saveEventForChildComponent.next({ eventType: 'SAVE_COMPLETE', data: res.programmedElement });
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
        RO.componentData = this.component.componentData;
        RO.componentRefId = this.component.componentRefId;
        RO.programmedElement = data.data;
        return RO;
    }


}
