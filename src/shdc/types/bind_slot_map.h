#pragma once
#include <string>
#include <array>
#include "fmt/format.h"
#include "consts.h"
#include "bindslot.h"
#include "shader_stage.h"
#include "slang.h"

namespace shdc {

// sokol-to-backend bind slot mappings for one shader stage
struct BindSlotMap {
    std::array<BindSlot, MaxUniformBlocks> uniform_blocks;
    std::array<BindSlot, MaxViews> views;
    std::array<BindSlot, MaxSamplers> samplers;

    bool add(const BindSlot& bindslot, std::string& out_error_msg);
    void allocate_backend_slots(ShaderStage::Enum stage);
    int find_uniformblock_slang_slot(const std::string& name, Slang::Enum slang) const;
    int find_view_slang_slot(const std::string& name, Slang::Enum slang) const;
    int find_sampler_slang_slot(const std::string& name, Slang::Enum slang) const;
    int find_uniformblock_index(const std::string& name) const;
    int find_view_index(const std::string& name) const;
    int find_sampler_index(const std::string& name) const;
    const BindSlot* find_uniformblock_bindslot(const std::string& name) const;
    const BindSlot* find_view_bindslot(const std::string& name) const;
    const BindSlot* find_sampler_bindslot(const std::string& name) const;

    void dump_debug() const;
};

inline int BindSlotMap::find_uniformblock_index(const std::string& name) const {
    for (int i = 0; i < MaxUniformBlocks; i++) {
        if (!uniform_blocks[i].empty() && (uniform_blocks[i].name == name)) {
            return i;
        }
    }
    return -1;
}

inline int BindSlotMap::find_view_index(const std::string& name) const {
    for (int i = 0; i < MaxViews; i++) {
        if (!views[i].empty() && (views[i].name == name)) {
            return i;
        }
    }
    return -1;
}

inline int BindSlotMap::find_sampler_index(const std::string& name) const {
    for (int i = 0; i < MaxSamplers; i++) {
        if (!samplers[i].empty() && (samplers[i].name == name)) {
            return i;
        }
    }
    return -1;
}

inline const BindSlot* BindSlotMap::find_uniformblock_bindslot(const std::string& name) const {
    int i = find_uniformblock_index(name);
    if (-1 != i) {
        return &uniform_blocks[i];
    } else {
        return nullptr;
    }
}

inline const BindSlot* BindSlotMap::find_view_bindslot(const std::string& name) const {
    int i = find_view_index(name);
    if (-1 != i) {
        return &views[i];
    } else {
        return nullptr;
    }
}

inline const BindSlot* BindSlotMap::find_sampler_bindslot(const std::string& name) const {
    int i = find_sampler_index(name);
    if (-1 != i) {
        return &samplers[i];
    } else {
        return nullptr;
    }
}

inline int BindSlotMap::find_uniformblock_slang_slot(const std::string& name, Slang::Enum slang) const {
    const BindSlot* bs = find_uniformblock_bindslot(name);
    if (bs) {
        return bs->get_slot_by_slang(slang);
    } else {
        return -1;
    }
}

inline int BindSlotMap::find_view_slang_slot(const std::string& name, Slang::Enum slang) const {
    const BindSlot* bs = find_view_bindslot(name);
    if (bs) {
        return bs->get_slot_by_slang(slang);
    } else {
        return -1;
    }
}

inline int BindSlotMap::find_sampler_slang_slot(const std::string& name, Slang::Enum slang) const {
    const BindSlot* bs = find_sampler_bindslot(name);
    if (bs) {
        return bs->get_slot_by_slang(slang);
    } else {
        return -1;
    }
}

inline bool BindSlotMap::add(const BindSlot& bindslot, std::string& out_error_msg) {
    const int binding = bindslot.binding;
    if (bindslot.type == BindSlot::Type::UniformBlock) {
        if  ((binding < 0) || (binding >= MaxUniformBlocks)) {
            out_error_msg = fmt::format("Uniform block {} binding {} out of range (must be 0..{})", bindslot.name, binding, MaxUniformBlocks - 1);
            return false;
        }
        if (uniform_blocks[binding].empty()) {
            uniform_blocks[binding] = bindslot;
        } else {
            out_error_msg = fmt::format("Uniform blocks {} and {} can't have the same binding {}", bindslot.name, uniform_blocks[binding].name, binding);
            return false;
        }
    } else if (bindslot.type == BindSlot::Type::Sampler) {
        if ((binding < 0) || (binding >= MaxSamplers)) {
            out_error_msg = fmt::format("Sampler {} binding {} out of range (must be 0..{})", bindslot.name, binding, MaxSamplers - 1);
            return false;
        }
        if (samplers[binding].empty()) {
            samplers[binding] = bindslot;
        } else {
            out_error_msg = fmt::format("Samplers {} and {} can't have the same binding {}", bindslot.name, samplers[binding].name, binding);
            return false;
        }
    } else {
        if ((binding < 0) || (binding >= MaxViews)) {
            out_error_msg = fmt::format("Resource {} binding {} out of range (must be 0..{})", bindslot.name, binding, MaxViews - 1);
            return false;
        }
        if (views[binding].empty()) {
            views[binding] = bindslot;
        } else {
            out_error_msg = fmt::format("Resources {} and {} can't have the same binding {}", bindslot.name, views[binding].name, binding);
            return false;
        }
    }
    return true;
}

inline void BindSlotMap::allocate_backend_slots(ShaderStage::Enum stage) {
    for (int i = 0; i < MaxUniformBlocks; i++) {
        auto& slot = uniform_blocks[i];
        if (slot.type != BindSlot::Type::Invalid) {
            slot.hlsl.register_b_n = i;
            slot.msl.buffer_n = i;
            slot.wgsl.group0_binding_n = (stage == ShaderStage::Fragment) ? MaxUniformBlocks + i : i;
        }
    }
    int hlsl_register_t_n = 0;
    int hlsl_register_u_n = 0;
    int msl_buffer_n = MaxUniformBlocks;
    int msl_texture_n = 0;
    int wgsl_group1_binding_n = (stage == ShaderStage::Fragment) ? 64 : 0;
    int glsl_storage_image_binding_n = 0;
    for (int i = 0; i < MaxViews; i++) {
        auto& slot = views[i];
        if (slot.type != BindSlot::Type::Invalid) {
            slot.wgsl.group1_binding_n = wgsl_group1_binding_n++;
        }
        switch (slot.type) {
            case BindSlot::Type::Texture:
                slot.hlsl.register_t_n = hlsl_register_t_n++;
                slot.msl.texture_n = msl_texture_n++;
                break;
            case BindSlot::Type::StorageBuffer:
                if (slot.readonly()) {
                    slot.hlsl.register_t_n = hlsl_register_t_n++;
                } else {
                    slot.hlsl.register_u_n = hlsl_register_u_n++;
                }
                slot.msl.buffer_n = msl_buffer_n++;
                // FIXME: must be < MaxStorageBufferBindingsPerStage!
                // FIXME: this is a problem on min-spec GL drivers with only 8 storage buffer bindslots!
                slot.glsl.binding_n = i;
                break;
            case BindSlot::Type::StorageImage:
                slot.hlsl.register_u_n = hlsl_register_u_n++;
                slot.msl.texture_n = msl_texture_n++;
                // this is fine since storage images are only allowed on compute stage
                slot.glsl.binding_n = glsl_storage_image_binding_n++;
                break;
            default: break;
        }
    }
    for (int i = 0; i < MaxSamplers; i++) {
        auto& slot = samplers[i];
        if (slot.type != BindSlot::Type::Invalid) {
            slot.wgsl.group1_binding_n = wgsl_group1_binding_n++;
            slot.hlsl.register_s_n = i;
            slot.msl.sampler_n = i;
        }
    }
}

inline void BindSlotMap::dump_debug() const {
    for (const auto& ub: uniform_blocks) {
        if (!ub.empty()) {
            ub.dump_debug();
        }
    }
    for (const auto& view: views) {
        if (!view.empty()) {
            view.dump_debug();
        }
    }
    for (const auto& smp: samplers) {
        if (!smp.empty()) {
            smp.dump_debug();
        }
    }
}

} // namespace shdc

