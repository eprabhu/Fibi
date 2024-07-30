import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {AdminDashboardComponent} from './admin-dashboard.component';
import {RouterModule, Routes} from '@angular/router';
import { SharedModule } from '../shared/shared.module';
import { MatIconModule } from '@angular/material/icon';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { AdminDashboardService } from './admin-dashboard.service';
import { FormsModule } from '@angular/forms';
import { DataStoreService } from '../disclosure/services/data-store.service';
import { EntityDetailsModule } from '../disclosure/entity-details/entity-details.module';
import { ProjectOverviewComponent } from './project-overview/project-overview.component';
import { CKEditorModule } from '@ckeditor/ckeditor5-angular';
import { ProjectOverviewCommentsSliderComponent } from './project-overview-comments-slider/project-overview-comments-slider.component';

const routes: Routes = [{path: '', component: AdminDashboardComponent}];

@NgModule({
    declarations: [
        AdminDashboardComponent , ProjectOverviewComponent , ProjectOverviewCommentsSliderComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        SharedComponentModule,
        MatIconModule,
        FormsModule,
        SharedModule,
        EntityDetailsModule,
        CKEditorModule 
    ],
    providers: [AdminDashboardService, DataStoreService]
})
export class AdminDashboardModule {
}
