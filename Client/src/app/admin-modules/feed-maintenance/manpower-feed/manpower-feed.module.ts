import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ManpowerFeedComponent } from './manpower-feed.component';
import { RouterModule, Routes } from '@angular/router';
import { ManpowerFeedService } from './manpower-feed.service';

const routes: Routes = [{
  path: '', component: ManpowerFeedComponent,
  children: [
    { path: '', redirectTo: 'award-retrigger', pathMatch: 'full' },
    {
      path: 'award-retrigger',
      loadChildren: () => import('./award-retrigger/award-retrigger.module').then(m => m.AwardRetriggerModule)
    },
    {
      path: 'position-retrigger',
      loadChildren: () => import('./position-retrigger/position-retrigger.module').then(m => m.PositionRetriggerModule)
    },
    {
      path: 'manpower-lookup',
      loadChildren: () => import('./manpower-lookup/manpower-lookup.module').then(m => m.ManpowerLookupModule)
    },
    {
      path: 'cost-allocation',
      loadChildren: () => import('./cost-allocation-retrigger/cost-allocation-retrigger.module').then(m => m.CostAllocationRetriggerModule)
    },
  ],
}];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes)
  ],
  declarations: [ManpowerFeedComponent],
  providers: [ManpowerFeedService]
})
export class ManpowerFeedModule { }
