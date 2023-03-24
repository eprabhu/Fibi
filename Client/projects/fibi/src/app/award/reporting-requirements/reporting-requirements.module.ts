import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReportingRequirementsComponent } from './reporting-requirements.component';
import { RouterModule } from '@angular/router';
import { SharedModule } from '../../shared/shared.module';
import { FormsModule } from '@angular/forms';
import { ReportingRequirementsService } from './reporting-requirements.service';
import { ReportingRequirementsEditComponent } from './reporting-requirements-edit/reporting-requirements-edit.component';
import { ReportingRequirementsViewComponent } from './reporting-requirements-view/reporting-requirements-view.component';
import { ReportingRequirementDetailsComponent } from './reporting-requirement-details/reporting-requirement-details.component';
import { SharedComponentModule } from '../../shared-component/shared-component.module';

@NgModule({
    imports: [
        CommonModule,
        RouterModule.forChild([{path: '', component: ReportingRequirementsComponent}]),
        SharedModule,
        SharedComponentModule,
        FormsModule
    ],
    declarations: [
        ReportingRequirementsComponent,
        ReportingRequirementsEditComponent,
        ReportingRequirementsViewComponent,
        ReportingRequirementDetailsComponent
    ],
    providers: [ReportingRequirementsService]
})
export class ReportingRequirementsModule {
}
