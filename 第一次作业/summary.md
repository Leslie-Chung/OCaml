# A circuit-level perspective of the optimum gate oxide thickness

最佳栅氧化层厚度的电路级透视图

## 解决什么问题

MOSFET的栅电极和沟道区之间的二氧化硅（SiO）层的厚度是当今硅集成电路中的最小尺寸。栅氧化层厚度的缩放是实现未来千兆级集成（GSI）的最限制性障碍之一。《国际半导体技术路线图》（ITRS）预测，预计2005年100纳米技术的有效厚度将在1.0-1.5纳米范围内。然而，ITRS强调，目前还没有这种预测的“已知解决方案”。为了实现这种积极的有效扩展，ITRS假设开发新的创新，例如用高栅极电介质取代SiO。这可能是一个有效的假设，因为半导体行业已经克服了许多以前的障碍来维持摩尔定律。由于SiO的CEA替代物尚未被接受，并且将该替代物集成到2005年制造工艺的时间间隔很短，因此理解的缩放限制对未来集成电路设计至关重要。

缩放的栅氧化层厚度

## 引入什么概念

引入了性能约束最小功率区域优化，以从电路级的角度投影物理栅氧化层厚度（OX）缩放限制。

## 提出什么算法

电路优化基于最近的物理阿尔法幂律MOSFET模型，该模型能够预测未来几代技术的CMOS电路性能。该模型用于导出包含过渡时间效应的传播延迟方程。还导出了物理紧凑栅隧穿电流模型来分析超薄氧化物层。



# Accurate estimation of total leakage in nanometer-scale bulk CMOS circuits based on device geometry and doping profile

基于器件几何形状和掺杂分布的纳米级体CMOS电路总泄漏的精确估计

## 解决什么问题

缩放器件中亚阈值、栅极和反向偏置结带到带隧穿（BTBT）泄漏的急剧增加导致逻辑电路中总泄漏功率的急剧增加。

## 引入什么概念

基于纳米尺度体CMOS器件中不同漏电流的紧凑建模，开发了一种精确估计逻辑电路中总漏电流的方法。基于器件几何形状、二维掺杂分布和工作温度建立了电流模型。建立了结BTBT泄漏的电路级模型。提出了亚阈值电流和栅极电流的简单模型。此外，还分析了衬底电子的量子力学行为对电路泄漏的影响。使用紧凑电流模型，晶体管被建模为电流源之和（SCS）。SCS晶体管模型已用于在室温和高温下估算简单逻辑门和复杂逻辑电路（采用25nm有效长度的晶体管设计）中的总泄漏。

## 提出什么算法

同上





# Exact and Heuristic Approaches to Input Vector Control for Leakage Power Reduction

减少泄漏功率的输入矢量控制的精确和启发式方法

## 解决什么问题

泄漏功耗在超大规模集成电路中是一个日益严重的问题，特别是在便携式应用中。

## 引入什么概念

研究了采用输入矢量控制（IVC）的静态互补金属氧化物半导体电路中泄漏功率最小化的两种新方法。

## 提出什么算法

作者通过伪布尔函数模拟泄漏效应。这些函数被线性化并合并到精确（最优）整数线性规划（ILP）模型中，称为虚拟门ILP，该模型分析相对于电路输入向量的泄漏变化。本文还提出了一种启发式混合整数线性规划（MLP）方法，该方法具有速度快、精度可快速估计、易于在运行时间和最优性之间进行权衡等优点。此外，MLP模型还提供了一种估计电路泄漏电流下限的方法。所提出的方法用于生成一系列关于减少泄漏的实验结果。



# Leakage current mechanisms and leakage reduction techniques in deep-submicrometer CMOS circuits

深亚微米CMOS电路中的漏电流机制和减少漏电流技术

## 解决什么问题

随着阈值电压、沟道长度和栅氧化层厚度的减小，深亚微米区域的高漏电流正成为CMOS电路功耗的重要因素。因此，不同泄漏组件的识别和建模对于估计和减少泄漏功率非常重要，特别是对于低功率应用。

## 引入什么概念

本文综述了各种晶体管本征泄漏机制，包括弱反转、漏极诱导势垒降低、栅极诱导漏极泄漏和栅极氧化物隧穿。解释了沟道工程技术，包括逆行掺杂和晕掺杂，作为管理短沟道效应的手段，用于连续缩放CMOS器件。

## 提出什么算法

无。属于介绍型文章



### Channel Engineering for Leakage Reduction

Retrograde Doping：逆行掺杂

Halo Doping：晕掺杂



### Circuit Techniques for Leakage Reduction 减少泄漏的电路技术

Standby Leakage Control Using Transistor Stacks：使用晶体管堆的备用泄漏控制

Multiple V~th~ Designs：多重V~th~(阈值电压)设计

Dynamic V~th~ Designs：动态V~th~设计

Supply Voltage Scaling：电源电压定标

Leakage Reduction Methods for Cache Memory：高速缓冲存储器的泄漏减少方法



# Ultrathin hafnium oxide with low leakage and excellent reliability for alternative gate dielectric application

漏电流低且可靠性高的超薄氧化铪，适用于替代栅极介质应用

## 解决什么问题

未来的栅极电介质应用需要具有高介电常数、具有良好的带对准的大带隙、低界面态密度和良好的热稳定性的栅极介电材料。不幸的是，许多高k材料，如Ta~2~O~5~, TiO~2~,  SrTiO~3~, and BaSrTiO~3~与硅直接接触时热不稳定，需要额外的阻挡层，这可能会增加工艺复杂性并施加厚度缩放限制。

## 引入什么概念

首次研究了作为替代栅介质的超薄HfO~2~的物理、电学和可靠性特性。优化了氧调制直流磁控溅射的关键工艺参数，以在不扣除量子力学效应的情况下实现11.5A的等效氧化物厚度（EOT）。优异的介电性能，如高介电常数、低漏电流、良好的热稳定性、可忽略的色散和良好的可靠性。

## 提出什么算法

无