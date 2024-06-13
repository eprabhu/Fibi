import { NotesAttachmentsModule } from './../user-dashboard/notes/notes-attachments.module';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ConsultingFormComponent } from './consulting-form.component';
import { SharedModule } from '../shared/shared.module';
import { RouterModule, Routes } from '@angular/router';
import { ResolveServiceService } from './services/resolve-service.service';
import { RouteGuardService } from './services/route-guard.service';
import { ReviewRouteGuardService } from './services/review-route-guard.service';
import { ConsultingService } from './services/consulting-service.service';
import { DataStoreService } from './services/data-store.service';
import { FormsModule } from '@angular/forms';
import { ReviewModule } from './review/review.module';
import { HistoryModule } from './history/history.module';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { HeaderSliderComponent } from './header-slider/header-slider.component';
import { SfiModule } from '../disclosure/sfi/sfi.module';
import { AttachmentsModule } from '../user-dashboard/attachments/attachments.module';

const routes: Routes = [{
    path: '', component: ConsultingFormComponent, canActivate: [ResolveServiceService], children: [
        {path: '', redirectTo: 'form', pathMatch: 'full'},
        {
            path: 'form',
            canDeactivate: [RouteGuardService],
            loadChildren: () => import('./form/form.module').then(m => m.FormModule)
        },
        {
            path: 'review',
            canDeactivate: [RouteGuardService],
            canActivate: [ReviewRouteGuardService],
            loadChildren: () => import('./review/review.module').then(m => m.ReviewModule)
        },
        {
            path: 'history',
            canDeactivate: [RouteGuardService],
            loadChildren: () => import('./history/history.module').then(m => m.HistoryModule)
        }
    ]
}];

@NgModule({
  declarations: [
    ConsultingFormComponent,
    HeaderSliderComponent
  ],
  imports: [
    CommonModule,
    SharedModule,
    RouterModule.forChild(routes),
    FormsModule,
    ReviewModule,
    HistoryModule,
    SharedComponentModule,
    NotesAttachmentsModule,
    AttachmentsModule,
    SfiModule
  ],
  providers: [
    ResolveServiceService,
    ReviewRouteGuardService,
    RouteGuardService,
    ConsultingService,
    DataStoreService
  ]
})
export class ConsultingFormModule { }
