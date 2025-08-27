#pragma once
#include "types/bindslot.h"
#include "uniform_block.h"
#include "texture.h"
#include "sampler.h"
#include "texture_sampler.h"
#include "storage_buffer.h"
#include "storage_image.h"

namespace shdc::refl {

struct Bindings {
    struct View {
        BindSlot::Type type = BindSlot::Invalid;
        struct Texture texture;
        struct StorageBuffer storage_buffer;
        struct StorageImage storage_image;
    };

    std::vector<UniformBlock> uniform_blocks;
    std::vector<StorageBuffer> storage_buffers;
    std::vector<StorageImage> storage_images;
    std::vector<Texture> textures;
    std::vector<Sampler> samplers;
    std::vector<TextureSampler> texture_samplers;

    View get_view_by_sokol_slot(int slot) const;
    int get_num_populated_views() const;

    const UniformBlock* find_uniform_block_by_sokol_slot(int slot) const;
    const StorageBuffer* find_storage_buffer_by_sokol_slot(int slot) const;
    const StorageImage* find_storage_image_by_sokol_slot(int slot) const;
    const Texture* find_texture_by_sokol_slot(int slot) const;
    const Sampler* find_sampler_by_sokol_slot(int slot) const;
    const TextureSampler* find_texture_sampler_by_sokol_slot(int slot) const;

    const UniformBlock* find_uniform_block_by_name(const std::string& name) const;
    const StorageBuffer* find_storage_buffer_by_name(const std::string& name) const;
    const StorageImage* find_storage_image_by_name(const std::string& name) const;
    const Texture* find_texture_by_name(const std::string& name) const;
    const Sampler* find_sampler_by_name(const std::string& name) const;
    const TextureSampler* find_texture_sampler_by_name(const std::string& name) const;

    void dump_debug(const std::string& indent) const;
};

inline Bindings::View Bindings::get_view_by_sokol_slot(int slot) const {
    View view;
    const Texture* tex = find_texture_by_sokol_slot(slot);
    const StorageBuffer* sbuf = find_storage_buffer_by_sokol_slot(slot);
    const StorageImage* simg = find_storage_image_by_sokol_slot(slot);
    if (tex) {
        assert((sbuf == nullptr) && (simg == nullptr));
        view.type = BindSlot::Type::Texture;
        view.texture = *tex;
    } else if (sbuf) {
        assert((tex == nullptr) && (simg == nullptr));
        view.type = BindSlot::Type::StorageBuffer;
        view.storage_buffer = *sbuf;
    } else if (simg) {
        assert((tex == nullptr) && (sbuf == nullptr));
        view.type = BindSlot::Type::StorageImage;
        view.storage_image = *simg;
    }
    return view;
}

inline int Bindings::get_num_populated_views() const {
    int num_views = 0;
    for (int slot = 0; slot < MaxViews; slot++) {
        const Texture* tex = find_texture_by_sokol_slot(slot);
        const StorageBuffer* sbuf = find_storage_buffer_by_sokol_slot(slot);
        const StorageImage* simg = find_storage_image_by_sokol_slot(slot);
        if (tex || sbuf || simg) {
            num_views++;
        }
    }
    return num_views;
}

inline const UniformBlock* Bindings::find_uniform_block_by_sokol_slot(int slot) const {
    for (const UniformBlock& ub: uniform_blocks) {
        if (ub.sokol_slot == slot) {
            return &ub;
        }
    }
    return nullptr;
}

inline const StorageBuffer* Bindings::find_storage_buffer_by_sokol_slot(int slot) const {
    for (const StorageBuffer& sbuf: storage_buffers) {
        if (sbuf.sokol_slot == slot) {
            return &sbuf;
        }
    }
    return nullptr;
}

inline const StorageImage* Bindings::find_storage_image_by_sokol_slot(int slot) const {
    for (const StorageImage& simg: storage_images) {
        if (simg.sokol_slot == slot) {
            return &simg;
        }
    }
    return nullptr;
}

inline const Texture* Bindings::find_texture_by_sokol_slot(int slot) const {
    for (const Texture& tex: textures) {
        if (tex.sokol_slot == slot) {
            return &tex;
        }
    }
    return nullptr;
}

inline const Sampler* Bindings::find_sampler_by_sokol_slot(int slot) const {
    for (const Sampler& smp: samplers) {
        if (smp.sokol_slot == slot) {
            return &smp;
        }
    }
    return nullptr;
}

inline const TextureSampler* Bindings::find_texture_sampler_by_sokol_slot(int slot) const {
    for (const TextureSampler& tex_smp: texture_samplers) {
        if (tex_smp.sokol_slot == slot) {
            return &tex_smp;
        }
    }
    return nullptr;
}

inline const UniformBlock* Bindings::find_uniform_block_by_name(const std::string& name) const {
    for (const UniformBlock& ub: uniform_blocks) {
        if (ub.name == name) {
            return &ub;
        }
    }
    return nullptr;
}

inline const StorageBuffer* Bindings::find_storage_buffer_by_name(const std::string& name) const {
    for (const StorageBuffer& sbuf: storage_buffers) {
        if (sbuf.name == name) {
            return &sbuf;
        }
    }
    return nullptr;
}

inline const StorageImage* Bindings::find_storage_image_by_name(const std::string& name) const {
    for (const StorageImage& simg: storage_images) {
        if (simg.name == name) {
            return &simg;
        }
    }
    return nullptr;
}

inline const Texture* Bindings::find_texture_by_name(const std::string& name) const {
    for (const Texture& tex: textures) {
        if (tex.name == name) {
            return &tex;
        }
    }
    return nullptr;
}

inline const Sampler* Bindings::find_sampler_by_name(const std::string& name) const {
    for (const Sampler& smp: samplers) {
        if (smp.name == name) {
            return &smp;
        }
    }
    return nullptr;
}

inline const TextureSampler* Bindings::find_texture_sampler_by_name(const std::string& name) const {
    for (const TextureSampler& tex_smp: texture_samplers) {
        if (tex_smp.name == name) {
            return &tex_smp;
        }
    }
    return nullptr;
}

inline void Bindings::dump_debug(const std::string& indent) const {
    fmt::print(stderr, "{}uniform_blocks:\n", indent);
    for (const auto& ub: uniform_blocks) {
        ub.dump_debug(indent);
    }
    fmt::print(stderr, "{}storage_buffers:\n", indent);
    for (const auto& sbuf: storage_buffers) {
        sbuf.dump_debug(indent);
    }
    fmt::print(stderr, "{}storage_images:\n", indent);
    for (const auto& simg: storage_images) {
        simg.dump_debug(indent);
    }
    fmt::print(stderr, "{}textures:\n", indent);
    for (const auto& tex: textures) {
        tex.dump_debug(indent);
    }
    fmt::print(stderr, "{}samplers:\n", indent);
    for (const auto& smp: samplers) {
        smp.dump_debug(indent);
    }
    fmt::print(stderr, "{}texture_samplers:\n", indent);
    for (const auto& tex_smp: texture_samplers) {
        tex_smp.dump_debug(indent);
    }
}

} // namespace
